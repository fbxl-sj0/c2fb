#!/usr/bin/env python3
"""
Project: SQLite FreeBASIC port tools
File: c2fb_source_unit.py

Purpose:

    Generate body candidates with record layouts declared privately in a C
    implementation file available to the body translator.

    Responsibilities:

    - Build a type-only generic-header binding for the selected C source.
    - Apply shared and per-source compiler options in tree mode.
    - Bound concurrent work while keeping per-file outputs independent.
    - Filter tree inputs by source-relative patterns when requested.
    - Combine that binding with an optional FreeBASIC source index in a
      temporary overlay.
    - Translate and publish a C body candidate plus its type context.

This file intentionally does NOT contain:

    - C parsing or FreeBASIC lowering.
    - The C2FB target matrix or compilation database workflows.
"""

import argparse
from concurrent.futures import ThreadPoolExecutor
import fnmatch
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile


BODY_SOURCE_SUFFIXES = {".bas", ".bi"}
SKIP_DIRECTORY_NAMES = {"build", "out", ".git", "node_modules"}
MAX_SOURCE_TREE_FILES = 32768
# Bound compiler fan-out so large trees cannot launch unbounded workers.
MAX_SOURCE_TREE_JOBS = 32
UNIT_OPTION_KEYS = (
    "header_options",
    "body_options",
    "header_compiler_options",
    "body_compiler_options",
)
MANAGED_BODY_OPTIONS = (
    "--fb-source-directory",
    "--fb-source-dir",
    "--fb-source-recursive",
)
STATIC_OBJECT_BINDING = re.compile(
    r"^' c2fb static-object: ([A-Za-z_][A-Za-z_0-9]*)="
    r"([A-Za-z_][A-Za-z_0-9]*)$",
    re.MULTILINE,
)
THREAD_LOCAL_OBJECT_SKIP = re.compile(
    r"^' c2fb skipped [^\r\n]*thread-local[^\r\n]*: "
    r"([A-Za-z_][A-Za-z_0-9]*):",
    re.MULTILINE,
)


def resolve_existing_file(value, label):
    path = Path(value)
    try:
        path = path.resolve()
    except (OSError, RuntimeError) as error:
        raise ValueError(f"cannot resolve {label} path {path}: {error}") from error
    if not path.is_file():
        raise ValueError(f"{label} does not exist: {path}")
    return path


def resolve_output_file(value, label):
    path = Path(value)
    try:
        return path.resolve()
    except (OSError, RuntimeError) as error:
        raise ValueError(f"cannot resolve {label} path {path}: {error}") from error


def path_is_within(path, root):
    try:
        path.relative_to(root)
        return True
    except ValueError:
        return False


def load_unit_manifest(value):
    if value is None:
        return []
    path = resolve_existing_file(value, "source-unit manifest")
    try:
        manifest = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, json.JSONDecodeError) as error:
        raise ValueError(f"cannot read source-unit manifest {path}: {error}") from error
    if not isinstance(manifest, dict) or manifest.get("version") != 1:
        raise ValueError("source-unit manifest must be an object with version 1")
    if set(manifest) != {"version", "profiles"}:
        raise ValueError(
            "source-unit manifest accepts only version and profiles fields"
        )
    profiles = manifest["profiles"]
    if not isinstance(profiles, list):
        raise ValueError("source-unit manifest profiles must be a list")

    normalized_profiles = []
    allowed_keys = {"pattern", *UNIT_OPTION_KEYS}
    for profile_index, profile in enumerate(profiles):
        label = f"source-unit profile {profile_index}"
        if not isinstance(profile, dict) or "pattern" not in profile:
            raise ValueError(f"{label} must contain a source pattern")
        if set(profile) - allowed_keys:
            raise ValueError(f"{label} contains an unknown field")
        pattern = profile["pattern"]
        if not isinstance(pattern, str) or not pattern.strip():
            raise ValueError(f"{label} pattern must be a non-empty path pattern")
        pattern = pattern.replace("\\", "/").strip()
        if pattern.startswith("/") or ".." in pattern.split("/"):
            raise ValueError(f"{label} pattern must be relative to the source tree")
        normalized = {"pattern": pattern}
        for option_key in UNIT_OPTION_KEYS:
            values = profile.get(option_key, [])
            if not isinstance(values, list) or any(
                not isinstance(item, str) or not item or
                "\0" in item or "\n" in item or "\r" in item
                for item in values
            ):
                raise ValueError(f"{label} {option_key} must be a list of arguments")
            if option_key == "body_options":
                for item in values:
                    if any(
                        item == reserved or item.startswith(reserved + "=")
                        for reserved in MANAGED_BODY_OPTIONS
                    ):
                        raise ValueError(
                            f"{label} cannot set {item}; use the source-unit "
                            "FreeBASIC source-directory options"
                        )
            normalized[option_key] = values
        normalized_profiles.append(normalized)
    return normalized_profiles


def options_for_source_unit(relative_path, profiles):
    source_name = relative_path.as_posix()
    options = {key: [] for key in UNIT_OPTION_KEYS}
    for profile in profiles:
        if not fnmatch.fnmatchcase(source_name, profile["pattern"]):
            continue
        for option_key in UNIT_OPTION_KEYS:
            options[option_key].extend(profile[option_key])
    return options


def resolve_converter(value):
    text = str(value)
    path = Path(text)
    if path.is_absolute() or len(path.parts) > 1:
        return resolve_existing_file(text, "C2FB converter")
    located = shutil.which(text)
    if located is None:
        raise ValueError(f"C2FB converter is not on PATH: {text}")
    return Path(located).resolve()


def command_output(result, output_messages=None):
    if output_messages is not None:
        output_messages.append((result.stdout, result.stderr))
        return result.returncode
    if result.stdout:
        sys.stdout.write(result.stdout)
        sys.stdout.flush()
    if result.stderr:
        sys.stderr.write(result.stderr)
        sys.stderr.flush()
    return result.returncode


def copy_or_link(source, target):
    target.parent.mkdir(parents=True, exist_ok=True)
    try:
        os.symlink(source.resolve(), target)
    except (NotImplementedError, OSError):
        shutil.copy2(source, target)


def stage_binding_sources(base_directory, overlay_directory,
                          local_type_context, recursive):
    overlay_directory.mkdir(parents=True, exist_ok=True)
    if base_directory is not None:
        iterator = (
            base_directory.rglob("*")
            if recursive
            else base_directory.iterdir()
        )
        for source in sorted(iterator):
            if not source.is_file() or source.suffix.lower() not in BODY_SOURCE_SUFFIXES:
                continue
            relative = source.relative_to(base_directory)
            if any(part in SKIP_DIRECTORY_NAMES for part in relative.parts):
                continue
            if source.name == local_type_context.name:
                raise ValueError(
                    "local type-context filename conflicts with an existing "
                    f"FreeBASIC source: {source}"
                )
            copy_or_link(source, overlay_directory / relative)

    local_target = overlay_directory / local_type_context.name
    if local_target.exists():
        raise ValueError(
            f"local type-context filename conflicts in the source overlay: "
            f"{local_target.name}"
        )
    copy_or_link(local_type_context, local_target)


def ensure_type_context_include(candidate_path, type_context_name):
    source = candidate_path.read_text(encoding="utf-8", errors="replace")
    include_line = f'#include once "{type_context_name}"'
    if include_line in source:
        return

    lines = source.splitlines(keepends=True)
    include_positions = [
        index for index, line in enumerate(lines)
        if line.lstrip().startswith("#include once ")
    ]
    if include_positions:
        include_position = include_positions[-1] + 1
    else:
        comment_end = next(
            (
                index for index, line in enumerate(lines)
                if line.strip() == "'/"
            ),
            -1,
        )
        include_position = comment_end + 1
    lines.insert(include_position, include_line + "\n")
    candidate_path.write_text("".join(lines), encoding="utf-8")


def static_object_bindings(type_context_path):
    source = type_context_path.read_text(encoding="utf-8", errors="replace")
    bindings = {}
    for source_name, freebasic_name in STATIC_OBJECT_BINDING.findall(source):
        previous_name = bindings.get(source_name)
        if previous_name is not None and previous_name != freebasic_name:
            raise ValueError(
                "local type context maps one C static object to multiple "
                f"FreeBASIC names: {source_name}"
            )
        bindings[source_name] = freebasic_name
    return tuple(sorted(bindings.items()))


def unsupported_thread_local_objects(type_context_path):
    source = type_context_path.read_text(encoding="utf-8", errors="replace")
    return tuple(sorted(set(THREAD_LOCAL_OBJECT_SKIP.findall(source))))


def run_source_unit(arguments, output_messages=None):
    source = resolve_existing_file(arguments.source, "C source")
    converter = resolve_converter(arguments.converter)
    candidate = resolve_output_file(arguments.candidate, "candidate output")
    type_context = resolve_output_file(
        arguments.type_context
        if arguments.type_context
        else candidate.with_name(candidate.stem + "_types.bi"),
        "local type-context output",
    )

    if candidate == source or type_context == source:
        raise ValueError("generated outputs must not overwrite the C input")
    if candidate == type_context:
        raise ValueError("candidate and local type-context outputs must differ")
    if candidate.parent != type_context.parent:
        raise ValueError(
            "candidate and local type-context outputs must share a directory"
        )

    base_directory = None
    if arguments.fb_source_directory:
        base_directory = Path(arguments.fb_source_directory).resolve()
        if not base_directory.is_dir():
            raise ValueError(
                f"FreeBASIC source directory does not exist: {base_directory}"
            )
        for output_path in (candidate, type_context):
            try:
                output_path.relative_to(base_directory)
            except ValueError:
                continue
            raise ValueError(
                "generated outputs must be outside the indexed FreeBASIC "
                f"source directory: {base_directory}"
            )

    candidate.parent.mkdir(parents=True, exist_ok=True)
    source_pattern = arguments.emit_source_pattern or str(source)
    type_context_name = type_context.name

    try:
        with tempfile.TemporaryDirectory(
            prefix=".c2fb-source-unit-", dir=candidate.parent
        ) as temporary:
            staging = Path(temporary)
            staged_type_context = staging / type_context_name
            staged_candidate = staging / candidate.name
            overlay = staging / "fb-source"

            header_command = [
                str(converter),
                "generic-header",
                str(source),
                str(staged_type_context),
                *arguments.header_options,
                "--exclude=*",
                "--preserve-type-dependencies",
            ]
            # The body pass writes its own source objects after function
            # declarations, which keeps callback initializers callable.
            if "--include-global-definitions" not in arguments.body_options:
                header_command.extend([
                    "--include-static-objects",
                    "--private-static-objects",
                ])
            header_command.extend([
                "--emit-source=" + source_pattern,
                # Keep pointer fields in selected records valid when their
                # tags are completed in a filtered or unselected header.
                "--forward-decl=*",
                arguments.compiler,
                *arguments.header_compiler_options,
            ])
            if arguments.include_header_types:
                # Body candidates can access records defined by included
                # headers. The default source filter keeps unrelated header
                # declarations out, so opt in to their type layouts only.
                header_command.append("--emit-source=*")
            result = subprocess.run(
                header_command, capture_output=True, text=True, check=False
            )
            if command_output(result, output_messages) != 0:
                return result.returncode
            if not staged_type_context.is_file() or \
               staged_type_context.stat().st_size == 0:
                raise ValueError("C2FB produced no local type context")
            object_bindings = static_object_bindings(staged_type_context)
            unsupported_thread_locals = unsupported_thread_local_objects(
                staged_type_context
            )
            if unsupported_thread_locals:
                message = (
                    "c2fb source unit: unsupported thread-local C objects: "
                    + ", ".join(unsupported_thread_locals)
                    + "\n"
                )
                if output_messages is None:
                    sys.stdout.write(message)
                    sys.stdout.flush()
                else:
                    output_messages.append((message, ""))

            stage_binding_sources(
                base_directory, overlay, staged_type_context,
                arguments.fb_source_recursive,
            )
            body_command = [
                str(converter),
                "translate-simple-generic-c",
                str(source),
                str(staged_candidate),
                *arguments.body_options,
                *(
                    "--reuse-static-object=" + source_name + "=" + freebasic_name
                    for source_name, freebasic_name in object_bindings
                ),
                arguments.compiler,
                *arguments.body_compiler_options,
                "--fb-source-directory=" + str(overlay),
            ]
            if arguments.fb_source_recursive:
                body_command.append("--fb-source-recursive")
            result = subprocess.run(
                body_command, capture_output=True, text=True, check=False
            )
            if command_output(result, output_messages) != 0:
                return result.returncode
            if not staged_candidate.is_file() or staged_candidate.stat().st_size == 0:
                raise ValueError("C2FB produced no body candidate")
            ensure_type_context_include(staged_candidate, type_context_name)

            backup_type_context = staging / "previous-type-context"
            type_context_was_present = (
                type_context.exists() or type_context.is_symlink()
            )
            if type_context_was_present:
                shutil.copy2(
                    type_context, backup_type_context, follow_symlinks=False
                )

            type_context_published = False
            try:
                os.replace(staged_type_context, type_context)
                type_context_published = True
                os.replace(staged_candidate, candidate)
            except OSError:
                if type_context_published:
                    if type_context_was_present:
                        os.replace(backup_type_context, type_context)
                    elif type_context.exists() or type_context.is_symlink():
                        type_context.unlink()
                raise
    except OSError as error:
        raise ValueError(f"cannot stage C2FB source-unit outputs: {error}") from error

    if output_messages is None:
        print(f"c2fb source unit: wrote {type_context}")
        print(f"c2fb source unit: wrote {candidate}")
    else:
        output_messages.append((
            f"c2fb source unit: wrote {type_context}\n"
            f"c2fb source unit: wrote {candidate}\n",
            "",
        ))
    return 0


def run_source_tree(arguments):
    source_directory = Path(arguments.source).resolve()
    output_directory = resolve_output_file(arguments.candidate, "candidate directory")
    if not source_directory.is_dir():
        raise ValueError(f"C source directory does not exist: {source_directory}")
    source_patterns = []
    for pattern in arguments.source_patterns:
        normalized = pattern.replace("\\", "/").strip()
        if not normalized or normalized.startswith("/") or \
           ".." in normalized.split("/"):
            raise ValueError(
                "source patterns must be non-empty paths relative to the source tree"
            )
        source_patterns.append(normalized)
    unit_profiles = load_unit_manifest(arguments.unit_manifest)
    if path_is_within(output_directory, source_directory) or \
       path_is_within(source_directory, output_directory):
        raise ValueError(
            "candidate and source directories must not overlap"
        )

    base_directory = None
    if arguments.fb_source_directory:
        base_directory = Path(arguments.fb_source_directory).resolve()
        if not base_directory.is_dir():
            raise ValueError(
                f"FreeBASIC source directory does not exist: {base_directory}"
            )
        if path_is_within(output_directory, base_directory):
            raise ValueError(
                "candidate directory must be outside the indexed FreeBASIC "
                f"source directory: {base_directory}"
            )

    source_files = []
    for source in source_directory.rglob("*.c"):
        relative = source.relative_to(source_directory)
        if any(part in SKIP_DIRECTORY_NAMES for part in relative.parts):
            continue
        if source_patterns and not any(
            fnmatch.fnmatchcase(relative.as_posix(), pattern)
            for pattern in source_patterns
        ):
            continue
        if source.is_file():
            source_files.append(source)
    source_files.sort()
    if not source_files:
        if source_patterns:
            raise ValueError(
                "no C sources matched the selected source patterns in "
                f"{source_directory}: {', '.join(source_patterns)}"
            )
        raise ValueError(f"no C source files found in {source_directory}")
    if len(source_files) > MAX_SOURCE_TREE_FILES:
        raise ValueError(
            f"source tree contains more than {MAX_SOURCE_TREE_FILES} C files"
        )

    output_paths = set()
    for source in source_files:
        relative = source.relative_to(source_directory)
        candidate = (
            output_directory / relative.parent /
            f"{relative.stem}_candidates.bas"
        ).resolve()
        if candidate in output_paths:
            raise ValueError(f"multiple C sources map to candidate {candidate}")
        output_paths.add(candidate)

    output_directory.mkdir(parents=True, exist_ok=True)

    def process_source(source):
        relative = source.relative_to(source_directory)
        candidate = (
            output_directory / relative.parent /
            f"{relative.stem}_candidates.bas"
        )
        unit_arguments = argparse.Namespace(**vars(arguments))
        unit_arguments.source = source
        unit_arguments.candidate = candidate
        unit_arguments.type_context = None
        profile_options = options_for_source_unit(relative, unit_profiles)
        for option_key in UNIT_OPTION_KEYS:
            setattr(
                unit_arguments, option_key,
                [*getattr(arguments, option_key), *profile_options[option_key]],
            )
        output_messages = []
        try:
            result = run_source_unit(unit_arguments, output_messages)
        except (OSError, ValueError) as error:
            output_messages.append((
                "",
                f"c2fb source unit: {relative}: {error}\n",
            ))
            result = 1
        return relative, result, output_messages

    def report_result(result):
        nonlocal failures
        _, return_code, output_messages = result
        for output_text, error_text in output_messages:
            if output_text:
                sys.stdout.write(output_text)
            if error_text:
                sys.stderr.write(error_text)
        sys.stdout.flush()
        sys.stderr.flush()
        if return_code != 0:
            failures += 1

    failures = 0
    jobs = min(arguments.jobs, len(source_files))
    if jobs == 1:
        for source in source_files:
            report_result(process_source(source))
    else:
        with ThreadPoolExecutor(max_workers=jobs) as executor:
            for offset in range(0, len(source_files), jobs):
                batch = source_files[offset:offset + jobs]
                for result in executor.map(process_source, batch):
                    report_result(result)

    print(
        f"c2fb source tree: processed {len(source_files)} C files; "
        f"failed {failures}; jobs {jobs}"
    )
    return 1 if failures else 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("source", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument("--converter", required=True)
    parser.add_argument("--compiler", default="cc")
    parser.add_argument(
        "--tree", action="store_true",
        help="convert each .c file under source into a separate candidate",
    )
    parser.add_argument(
        "--source-pattern", dest="source_patterns", action="append", default=[],
        help="with --tree, select source-relative paths matching this pattern; repeat to add patterns",
    )
    parser.add_argument(
        "--jobs", type=int, default=1,
        help=(
            "with --tree, run this many independent C source units concurrently "
            f"(1-{MAX_SOURCE_TREE_JOBS}; default: 1)"
        ),
    )
    parser.add_argument("--fb-source-directory")
    parser.add_argument("--fb-source-recursive", action="store_true")
    parser.add_argument(
        "--unit-manifest", type=Path,
        help="JSON per-C-file options used with --tree",
    )
    parser.add_argument("--type-context", type=Path)
    parser.add_argument("--emit-source-pattern")
    parser.add_argument(
        "--include-header-types", action="store_true",
        help=(
            "include record, typedef, and enum definitions from all headers "
            "in the type-only context"
        ),
    )
    parser.add_argument(
        "--header-option", dest="header_options", action="append", default=[]
    )
    parser.add_argument(
        "--body-option", dest="body_options", action="append", default=[]
    )
    parser.add_argument(
        "--header-compiler-option", dest="header_compiler_options",
        action="append", default=[],
    )
    parser.add_argument(
        "--body-compiler-option", dest="body_compiler_options",
        action="append", default=[],
    )
    arguments = parser.parse_args()

    reserved_body_options = (
        "--fb-source-directory",
        "--fb-source-dir",
        "--fb-source-recursive",
    )
    for option in arguments.body_options:
        if any(
            option == reserved or option.startswith(reserved + "=")
            for reserved in reserved_body_options
        ):
            parser.error(
                f"{option} is managed by c2fb_source_unit.py; use its "
                "FreeBASIC source-directory options"
            )
    if arguments.source_patterns and not arguments.tree:
        parser.error("--source-pattern requires --tree")
    if arguments.jobs < 1 or arguments.jobs > MAX_SOURCE_TREE_JOBS:
        parser.error(f"--jobs must be between 1 and {MAX_SOURCE_TREE_JOBS}")
    if arguments.jobs != 1 and not arguments.tree:
        parser.error("--jobs greater than 1 requires --tree")
    try:
        if arguments.tree:
            if arguments.type_context is not None:
                parser.error("--type-context is not available with --tree")
            return run_source_tree(arguments)
        if arguments.unit_manifest is not None:
            parser.error("--unit-manifest requires --tree")
        return run_source_unit(arguments)
    except ValueError as error:
        print(f"c2fb source unit: {error}", file=sys.stderr)
        return 2
    except OSError as error:
        print(f"c2fb source unit: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())


# end of c2fb_source_unit.py
