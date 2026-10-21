"""
Project: C2FB generic body conversion
File: c2fb_compile_commands.py
Purpose: Convert C translation units using their compilation database settings.
Responsibilities: Read compile_commands.json, retain preprocessing flags, and
run C2FB once for each distinct C compiler invocation. Optionally generate and
include a private type context for each translation unit, with bounded parallel
workers and staged publication.
This file intentionally does NOT contain: C parsing or FreeBASIC translation.
"""

import argparse
from concurrent.futures import ThreadPoolExecutor
import csv
from dataclasses import dataclass, replace
import hashlib
import json
import os
from pathlib import Path
import re
import shlex
import subprocess
import sys
import tempfile
from typing import Optional, Tuple


MAX_RESPONSE_DEPTH = 16
# Bound concurrent C2FB processes when processing a large compilation database.
MAX_COMPILE_COMMAND_JOBS = 32
COMPILER_LAUNCHERS = {"ccache", "sccache", "distcc", "icecc"}
DROP_FLAGS = {
    "-c",
    "-S",
    "-E",
    "-M",
    "-MM",
    "-MD",
    "-MMD",
    "-MG",
    "-MP",
    "-fsyntax-only",
    "-save-temps",
    "-ftime-trace",
    "-shared",
    "-static",
    "-pie",
    "-rdynamic",
}
DROP_VALUE_OPTIONS = {
    "-o",
    "-MF",
    "-MT",
    "-MQ",
    "-MJ",
    "-L",
    "-l",
    "--serialize-diagnostics",
    "-Xlinker",
    "-framework",
    "-weak_framework",
    "-x",
}
PAIRED_OPTIONS = {
    "-I",
    "-isystem",
    "-iquote",
    "-idirafter",
    "-F",
    "-iframework",
    "-include",
    "-imacros",
    "-D",
    "-U",
    "-target",
    "--target",
    "--sysroot",
    "-isysroot",
    "-Xclang",
    "-Xpreprocessor",
    "-Xassembler",
    "-arch",
    "-stdlib",
    "-resource-dir",
    "-gcc-toolchain",
    "--gcc-toolchain",
}
LINKER_INPUT_SUFFIXES = {
    ".o",
    ".obj",
    ".a",
    ".so",
    ".dylib",
    ".dll",
    ".lib",
    ".lo",
    ".bc",
}


@dataclass(frozen=True)
class CompileCommandModel:
    directory: Path
    source_argument: str
    source_path: Path
    compiler: str
    compiler_arguments: Tuple[str, ...]


@dataclass(frozen=True)
class PreparedCompileCommandModel:
    command: CompileCommandModel
    signature: str
    output_name: str


def require_text(value, label):
    if not isinstance(value, str) or not value.strip() or "\0" in value:
        raise ValueError(f"{label} must be a non-empty string")
    return value.strip()


def resolve_path(base_directory, value, label):
    path = Path(require_text(value, label))
    if not path.is_absolute():
        path = base_directory / path
    try:
        return path.resolve()
    except (OSError, RuntimeError) as error:
        raise ValueError(f"cannot resolve {label} path {path}: {error}") from error


def expand_response_files(arguments, working_directory, stack=(), depth=0):
    if depth >= MAX_RESPONSE_DEPTH:
        raise ValueError("compiler response-file nesting exceeds its configured limit")
    result = []
    for argument in arguments:
        if not argument.startswith("@") or argument == "@":
            result.append(argument)
            continue
        response_path = Path(argument[1:])
        if not response_path.is_absolute():
            response_path = working_directory / response_path
        try:
            response_path = response_path.resolve()
        except (OSError, RuntimeError) as error:
            raise ValueError(f"cannot resolve compiler response path {response_path}: {error}") from error
        if response_path in stack:
            raise ValueError(f"cycle in compiler response files: {response_path}")
        try:
            response_text = response_path.read_text(encoding="utf-8")
        except OSError as error:
            raise ValueError(f"cannot read compiler response file {response_path}: {error}") from error
        try:
            response_arguments = shlex.split(response_text, posix=True)
        except ValueError as error:
            raise ValueError(f"cannot parse compiler response file {response_path}: {error}") from error
        result.extend(
            expand_response_files(
                response_arguments,
                working_directory,
                stack + (response_path,),
                depth + 1,
            )
        )
    return result


def argument_is_source(argument, source_path, working_directory):
    if argument.startswith("-") or argument == "":
        return False
    candidate = Path(argument)
    if not candidate.is_absolute():
        candidate = working_directory / candidate
    try:
        return candidate.resolve() == source_path
    except (OSError, RuntimeError):
        return False


def argument_is_linker_input(argument):
    return Path(argument).suffix.lower() in LINKER_INPUT_SUFFIXES


def preprocess_arguments(arguments, source_path, working_directory):
    result = []
    position = 0
    while position < len(arguments):
        argument = arguments[position]
        if argument in DROP_FLAGS:
            position += 1
            continue
        if argument in DROP_VALUE_OPTIONS:
            if position + 1 >= len(arguments):
                raise ValueError(f"compiler option {argument} is missing its value")
            position += 2
            continue
        if argument in PAIRED_OPTIONS:
            if position + 1 >= len(arguments):
                raise ValueError(f"compiler option {argument} is missing its value")
            result.extend((argument, arguments[position + 1]))
            position += 2
            continue
        if (
            argument.startswith("--output=")
            or argument.startswith("-MF")
            or argument.startswith("-MT")
            or argument.startswith("-MQ")
            or argument.startswith("-MJ")
        ):
            position += 1
            continue
        if argument.startswith("-o") and len(argument) > 2:
            output_suffix = Path(argument[2:]).suffix.lower()
            if output_suffix in {".o", ".obj", ".s", ".d", ".json"}:
                position += 1
                continue
        if (
            argument.startswith("-Wl,")
            or argument.startswith("-l")
            or argument.startswith("-L")
        ):
            position += 1
            continue
        if argument_is_source(argument, source_path, working_directory):
            position += 1
            continue
        if argument_is_linker_input(argument):
            position += 1
            continue
        result.append(argument)
        position += 1
    return result


def parse_command_entry(entry, index, database_directory) -> Optional[CompileCommandModel]:
    if not isinstance(entry, dict):
        raise ValueError(f"compile command {index} must be a JSON object")
    directory = resolve_path(
        database_directory, entry.get("directory"), f"compile command {index} directory"
    )
    if not directory.is_dir():
        raise ValueError(f"compile command {index} directory does not exist: {directory}")
    source_argument = require_text(entry.get("file"), f"compile command {index} file")
    source_path = resolve_path(directory, source_argument, f"compile command {index} file")
    if source_path.suffix.lower() != ".c":
        return None
    if not source_path.is_file():
        raise ValueError(f"compile command {index} source does not exist: {source_path}")

    raw_arguments = entry.get("arguments")
    if raw_arguments is not None:
        if not isinstance(raw_arguments, list):
            raise ValueError(f"compile command {index} arguments must be a list")
        command = [require_text(value, f"compile command {index} argument") for value in raw_arguments]
    else:
        command_text = require_text(entry.get("command"), f"compile command {index} command")
        try:
            command = shlex.split(command_text, posix=True)
        except ValueError as error:
            raise ValueError(f"cannot parse compile command {index}: {error}") from error
    if not command:
        raise ValueError(f"compile command {index} is empty")

    compiler = command[0]
    compiler_arguments = command[1:]
    if Path(compiler).name.lower() in COMPILER_LAUNCHERS:
        if not compiler_arguments or compiler_arguments[0].startswith("-"):
            raise ValueError(
                f"compile command {index} uses a launcher without a direct compiler name"
            )
        compiler = compiler_arguments[0]
        compiler_arguments = compiler_arguments[1:]

    compiler_arguments = expand_response_files(compiler_arguments, directory)
    compiler_arguments = preprocess_arguments(
        compiler_arguments, source_path, directory
    )
    return CompileCommandModel(
        directory=directory,
        source_argument=source_argument,
        source_path=source_path,
        compiler=compiler,
        compiler_arguments=tuple(compiler_arguments),
    )


def command_signature(entry: CompileCommandModel, c2fb_options):
    signature_parts = [
        str(entry.directory),
        str(entry.source_path),
        entry.compiler,
        *entry.compiler_arguments,
        "--c2fb-options--",
        *c2fb_options,
    ]
    return json.dumps(signature_parts, ensure_ascii=True, separators=(",", ":"))


def output_filename(entry: CompileCommandModel, signature):
    source_stem = re.sub(r"[^A-Za-z0-9_.-]", "_", entry.source_path.stem)
    digest = hashlib.sha256(signature.encode("utf-8")).hexdigest()[:12]
    return f"{source_stem}_{digest}_candidates.bas"


def split_source_unit_options(options):
    body_options = []
    fb_source_directory = None
    fb_source_recursive = False
    index = 0
    while index < len(options):
        option = options[index]
        if option in {"--fb-source-directory", "--fb-source-dir"}:
            index += 1
            if index >= len(options):
                raise ValueError(f"{option} requires a directory")
            value = require_text(options[index], "FreeBASIC source directory")
            if fb_source_directory is not None and fb_source_directory != value:
                raise ValueError("conflicting FreeBASIC source directories")
            fb_source_directory = value
        elif option.startswith("--fb-source-directory=") or \
             option.startswith("--fb-source-dir="):
            value = require_text(option.split("=", 1)[1], "FreeBASIC source directory")
            if fb_source_directory is not None and fb_source_directory != value:
                raise ValueError("conflicting FreeBASIC source directories")
            fb_source_directory = value
        elif option == "--fb-source-recursive":
            fb_source_recursive = True
        elif option.startswith("--fb-source-recursive="):
            value = option.split("=", 1)[1].casefold()
            if value not in {"0", "1", "false", "true"}:
                raise ValueError("--fb-source-recursive must be 0 or 1")
            fb_source_recursive = value in {"1", "true"}
        else:
            body_options.append(option)
        index += 1
    return body_options, fb_source_directory, fb_source_recursive


def resolve_converter(converter_text, base_directory):
    converter_text = require_text(converter_text, "converter")
    converter_path = Path(converter_text)
    if converter_path.is_absolute() or len(converter_path.parts) > 1:
        return str(resolve_path(base_directory, converter_text, "converter"))
    return converter_text


def publish_staged_outputs(staged_outputs, backup_directory):
    """Publish one compile-command batch, restoring old files on rename failure."""
    staged_outputs = list(staged_outputs)
    destinations = set()
    for _, final_path in staged_outputs:
        final_path = Path(final_path)
        destination = os.path.normcase(os.path.abspath(final_path))
        if destination in destinations:
            raise OSError(f"duplicate compile-command output path: {final_path}")
        destinations.add(destination)
        if final_path.is_dir() and not final_path.is_symlink():
            raise IsADirectoryError(
                f"compile-command output is a directory: {final_path}"
            )

    backup_directory.mkdir(parents=True, exist_ok=True)
    backups = []
    published = []
    try:
        for output_index, (_, final_path) in enumerate(staged_outputs):
            final_path = Path(final_path)
            backup_path = None
            if final_path.exists() or final_path.is_symlink():
                backup_path = backup_directory / f"output-{output_index:08d}"
                os.replace(final_path, backup_path)
            backups.append((backup_path, final_path))

        for staged_path, final_path in staged_outputs:
            final_path = Path(final_path)
            os.replace(staged_path, final_path)
            published.append(final_path)
    except OSError as publish_error:
        rollback_errors = []
        for final_path in reversed(published):
            try:
                if final_path.exists() or final_path.is_symlink():
                    final_path.unlink()
            except OSError as rollback_error:
                rollback_errors.append(str(rollback_error))
        for backup_path, final_path in reversed(backups):
            if backup_path is None or not (
                backup_path.exists() or backup_path.is_symlink()
            ):
                continue
            try:
                os.replace(backup_path, final_path)
            except OSError as rollback_error:
                rollback_errors.append(str(rollback_error))
        if rollback_errors:
            raise OSError(
                f"publication failed ({publish_error}); rollback failed: "
                + "; ".join(rollback_errors)
            ) from publish_error
        raise


def run_compile_database(
    database_path,
    output_directory,
    converter,
    c2fb_options,
    source_unit_context=False,
    header_options=(),
    jobs=1,
):
    if not isinstance(jobs, int) or jobs < 1 or jobs > MAX_COMPILE_COMMAND_JOBS:
        print(
            f"c2fb compile commands: --jobs must be between 1 and "
            f"{MAX_COMPILE_COMMAND_JOBS}",
            file=sys.stderr,
        )
        return 2
    try:
        database_text = database_path.read_text(encoding="utf-8")
    except OSError as error:
        print(f"c2fb compile commands: cannot read {database_path}: {error}", file=sys.stderr)
        return 1
    try:
        entries = json.loads(database_text)
    except json.JSONDecodeError as error:
        print(
            f"c2fb compile commands: invalid JSON at line {error.lineno}, "
            f"column {error.colno}: {error.msg}",
            file=sys.stderr,
        )
        return 2
    if not isinstance(entries, list) or not entries:
        print("c2fb compile commands: database must be a non-empty JSON array", file=sys.stderr)
        return 2

    if source_unit_context:
        try:
            body_options, fb_source_directory, fb_source_recursive = \
                split_source_unit_options(c2fb_options)
        except ValueError as error:
            print(f"c2fb compile commands: {error}", file=sys.stderr)
            return 2
    else:
        if header_options:
            print(
                "c2fb compile commands: --header-option requires "
                "--source-unit-context",
                file=sys.stderr,
            )
            return 2
        body_options = c2fb_options
        fb_source_directory = None
        fb_source_recursive = False

    prepared_entries = []
    seen_signatures = set()
    skipped_non_c_count = 0
    try:
        for index, entry in enumerate(entries):
            parsed = parse_command_entry(entry, index, database_path.parent)
            if parsed is None:
                skipped_non_c_count += 1
                continue
            signature_options = list(c2fb_options)
            if source_unit_context:
                signature_options.extend(("--source-unit-context", *header_options))
            signature = command_signature(parsed, signature_options)
            if signature in seen_signatures:
                continue
            seen_signatures.add(signature)
            prepared_entries.append(
                PreparedCompileCommandModel(
                    command=parsed,
                    signature=signature,
                    output_name=output_filename(parsed, signature),
                )
            )
    except ValueError as error:
        print(f"c2fb compile commands: {error}", file=sys.stderr)
        return 2
    if not prepared_entries:
        print("c2fb compile commands: no C source commands were found", file=sys.stderr)
        return 2

    try:
        output_directory.mkdir(parents=True, exist_ok=True)
    except OSError as error:
        print(f"c2fb compile commands: cannot create {output_directory}: {error}", file=sys.stderr)
        return 1

    for entry in prepared_entries:
        final_path = output_directory / entry.output_name
        try:
            output_resolved = final_path.resolve()
        except (OSError, RuntimeError) as error:
            print(
                f"c2fb compile commands: cannot resolve output {final_path}: {error}",
                file=sys.stderr,
            )
            return 2
        if output_resolved == entry.command.source_path:
            print(f"c2fb compile commands: output would overwrite {final_path}", file=sys.stderr)
            return 2

    try:
        with tempfile.TemporaryDirectory(
            prefix=".c2fb-compile-commands-", dir=output_directory
        ) as temporary_directory:
            temporary_directory = Path(temporary_directory)
            staged_outputs = []

            def convert_entry(entry_index, entry):
                final_path = output_directory / entry.output_name
                staged_path = temporary_directory / entry.output_name
                staged_type_context = None
                final_type_context = None
                if source_unit_context:
                    type_context_name = Path(entry.output_name).stem + "_types.bi"
                    staged_type_context = temporary_directory / type_context_name
                    final_type_context = output_directory / type_context_name
                    source_unit_tool = Path(__file__).with_name("c2fb_source_unit.py")
                    command = [
                        sys.executable,
                        str(source_unit_tool),
                        entry.command.source_argument,
                        str(staged_path),
                        "--converter",
                        converter,
                        "--compiler",
                        entry.command.compiler,
                    ]
                    if fb_source_directory is not None:
                        command.extend((
                            "--fb-source-directory",
                            fb_source_directory,
                        ))
                    if fb_source_recursive:
                        command.append("--fb-source-recursive")
                    command.extend(
                        "--header-option=" + option
                        for option in header_options
                    )
                    command.extend(
                        "--body-option=" + option
                        for option in body_options
                    )
                    for compiler_option in entry.command.compiler_arguments:
                        command.append(
                            "--header-compiler-option=" + compiler_option
                        )
                        command.append(
                            "--body-compiler-option=" + compiler_option
                        )
                else:
                    command = [
                        converter,
                        "translate-simple-generic-c",
                        entry.command.source_argument,
                        str(staged_path),
                        *body_options,
                        entry.command.compiler,
                        *entry.command.compiler_arguments,
                    ]
                with tempfile.TemporaryFile(
                    mode="w+t", encoding="utf-8", errors="replace"
                ) as log_file:
                    try:
                        result = subprocess.run(
                            command,
                            cwd=entry.command.directory,
                            stdout=log_file,
                            stderr=log_file,
                            check=False,
                        )
                    except OSError as error:
                        return {
                            "entry_index": entry_index,
                            "entry": entry,
                            "final_path": final_path,
                            "log": "",
                            "error": (
                                f"could not start C2FB for "
                                f"{entry.command.source_path}: {error}"
                            ),
                            "status": 127,
                            "staged_outputs": [],
                        }
                    log_file.seek(0)
                    output_log = log_file.read()
                if staged_type_context is not None:
                    output_log = output_log.replace(
                        str(staged_type_context), str(final_type_context)
                    )
                output_log = output_log.replace(str(staged_path), str(final_path))
                if result.returncode != 0:
                    return {
                        "entry_index": entry_index,
                        "entry": entry,
                        "final_path": final_path,
                        "log": output_log,
                        "error": f"failed with status {result.returncode}",
                        "status": 1,
                        "staged_outputs": [],
                    }
                if not staged_path.is_file() or staged_path.stat().st_size == 0:
                    return {
                        "entry_index": entry_index,
                        "entry": entry,
                        "final_path": final_path,
                        "log": output_log,
                        "error": (
                            f"C2FB produced no candidate for "
                            f"{entry.command.source_path}"
                        ),
                        "status": 1,
                        "staged_outputs": [],
                    }
                entry_staged_outputs = []
                if staged_type_context is not None:
                    if not staged_type_context.is_file() or \
                       staged_type_context.stat().st_size == 0:
                        return {
                            "entry_index": entry_index,
                            "entry": entry,
                            "final_path": final_path,
                            "log": output_log,
                            "error": (
                                "C2FB produced no local type context for "
                                f"{entry.command.source_path}"
                            ),
                            "status": 1,
                            "staged_outputs": [],
                        }
                    entry_staged_outputs.append(
                        (staged_type_context, final_type_context)
                    )
                entry_staged_outputs.append((staged_path, final_path))
                return {
                    "entry_index": entry_index,
                    "entry": entry,
                    "final_path": final_path,
                    "log": output_log,
                    "error": "",
                    "status": 0,
                    "staged_outputs": entry_staged_outputs,
                }

            def report_result(result):
                entry = result["entry"]
                print(
                    f"c2fb compile commands: {result['entry_index'] + 1}/"
                    f"{len(prepared_entries)} {entry.command.source_path} -> "
                    f"{result['final_path']}",
                    flush=True,
                )
                if result["log"]:
                    sys.stdout.write(result["log"])
                    sys.stdout.flush()
                if result["status"] != 0:
                    print(
                        f"c2fb compile commands: {result['error']}; "
                        "no staged candidates were published",
                        file=sys.stderr,
                    )
                return result["status"]

            effective_jobs = min(jobs, len(prepared_entries))
            if effective_jobs == 1:
                for entry_index, entry in enumerate(prepared_entries):
                    result = convert_entry(entry_index, entry)
                    if report_result(result) != 0:
                        return result["status"]
                    staged_outputs.extend(result["staged_outputs"])
            else:
                with ThreadPoolExecutor(max_workers=effective_jobs) as executor:
                    for batch_start in range(0, len(prepared_entries), effective_jobs):
                        batch = [
                            (entry_index, prepared_entries[entry_index])
                            for entry_index in range(
                                batch_start,
                                min(batch_start + effective_jobs, len(prepared_entries)),
                            )
                        ]
                        for result in executor.map(
                            lambda item: convert_entry(item[0], item[1]), batch
                        ):
                            if report_result(result) != 0:
                                return result["status"]
                            staged_outputs.extend(result["staged_outputs"])

            staged_map_path = temporary_directory / "compile-command-map.csv"
            with staged_map_path.open("w", encoding="utf-8", newline="") as map_file:
                writer = csv.writer(map_file)
                writer.writerow(
                    (
                        "CandidateFile",
                        "SourceFile",
                        "WorkingDirectory",
                        "Compiler",
                        "CompilerArguments",
                        "C2FBOptions",
                        "SourceUnitContext",
                        "TypeContextFile",
                        "HeaderOptions",
                    )
                )
                for entry in prepared_entries:
                    type_context_name = ""
                    if source_unit_context:
                        type_context_name = Path(entry.output_name).stem + "_types.bi"
                    writer.writerow(
                        (
                            entry.output_name,
                            str(entry.command.source_path),
                            str(entry.command.directory),
                            entry.command.compiler,
                            json.dumps(entry.command.compiler_arguments, ensure_ascii=False),
                            json.dumps(c2fb_options, ensure_ascii=False),
                            "1" if source_unit_context else "0",
                            type_context_name,
                            json.dumps(list(header_options), ensure_ascii=False),
                        )
                    )
            staged_outputs.append(
                (staged_map_path, output_directory / "compile-command-map.csv")
            )

            publish_staged_outputs(
                staged_outputs, temporary_directory / "previous-output"
            )
    except OSError as error:
        print(f"c2fb compile commands: output error: {error}", file=sys.stderr)
        return 1

    print(
        f"c2fb compile commands: published {len(prepared_entries)} candidates in "
        f"{output_directory}; jobs {effective_jobs}; "
        f"skipped {skipped_non_c_count} non-C entries"
    )
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("database", type=Path)
    parser.add_argument("output_directory", type=Path)
    parser.add_argument(
        "--converter",
        default=str(Path(__file__).resolve().parent / "build" / "c2fb"),
        help="C2FB executable path or command name",
    )
    parser.add_argument(
        "--c2fb-option",
        action="append",
        default=[],
        help="C2FB body option, repeat for each option argument",
    )
    parser.add_argument(
        "--source-unit-context",
        action="store_true",
        help="generate a private type and static-object context for each C file",
    )
    parser.add_argument(
        "--jobs",
        type=int,
        default=1,
        help=(
            "run this many independent compile commands concurrently "
            f"(1-{MAX_COMPILE_COMMAND_JOBS}; default: 1)"
        ),
    )
    parser.add_argument(
        "--header-option",
        action="append",
        default=[],
        help="generic-header option for --source-unit-context; repeat as needed",
    )
    arguments = parser.parse_args()
    if arguments.jobs < 1 or arguments.jobs > MAX_COMPILE_COMMAND_JOBS:
        parser.error(
            f"--jobs must be between 1 and {MAX_COMPILE_COMMAND_JOBS}"
        )
    database_path = arguments.database.resolve()
    base_directory = database_path.parent
    try:
        output_directory = resolve_path(
            base_directory, str(arguments.output_directory), "output_directory"
        )
        converter = resolve_converter(arguments.converter, base_directory)
        c2fb_options = [
            require_text(option, "C2FB option") for option in arguments.c2fb_option
        ]
        header_options = [
            require_text(option, "C2FB header option")
            for option in arguments.header_option
        ]
    except ValueError as error:
        parser.error(str(error))
    return run_compile_database(
        database_path,
        output_directory,
        converter,
        c2fb_options,
        arguments.source_unit_context,
        header_options,
        arguments.jobs,
    )


if __name__ == "__main__":
    raise SystemExit(main())

# end of c2fb_compile_commands.py
