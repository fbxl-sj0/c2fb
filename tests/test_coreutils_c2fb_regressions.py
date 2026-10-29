#!/usr/bin/env python3
"""
Project: C2FB Coreutils integration regressions
File: test_coreutils_c2fb_regressions.py
Purpose: Verify C2FB fixes found while translating GNU Coreutils.
Responsibilities: Compare focused C source units with native and GCC
FreeBASIC output, including file-scope data and runtime callbacks.
This file intentionally does NOT contain: GNU Coreutils upstream tests or
the general C2FB regression suite.
"""

import argparse
import ctypes
from pathlib import Path
import shlex
import subprocess
import sys
import tempfile


def run(command, *, cwd):
    result = subprocess.run(
        list(map(str, command)), cwd=cwd, capture_output=True, text=True,
        check=False,
    )
    if result.returncode:
        raise AssertionError(
            f"Command failed: {shlex.join(map(str, command))}\n"
            f"{result.stdout}{result.stderr}"
        )
    return result.stdout


def translate_source_unit(converter, cc, source_directory, source, candidate,
                          options=()):
    source_unit_tool = Path(__file__).resolve().parents[1] / "c2fb_source_unit.py"
    return run([
        sys.executable, source_unit_tool, source, candidate,
        "--converter", converter, "--compiler", cc[0], *options,
    ], cwd=source_directory)


def compile_and_compare(name, source_directory, candidate, reference_path,
                        fbc, check):
    reference = ctypes.CDLL(str(reference_path))
    for backend, flags in (("native", []), ("gcc", ["-gen", "gcc"])):
        output = source_directory / f"{name}-{backend}.so"
        run(fbc + flags + ["-w", "all", "-dll", candidate, "-x", output],
            cwd=source_directory)
        check(reference, ctypes.CDLL(str(output)), backend)
    print(f"{name}: PASS on native and GCC backends")


def check_c23_static_booleans(converter, fbc, cc, work):
    source_directory = work / "c23-static-booleans"
    source_directory.mkdir()
    source = source_directory / "static_booleans.c"
    source.write_text(
        "#include <stdbool.h>\n"
        "static bool c2fb_enabled = true;\n"
        "static bool c2fb_disabled = false;\n"
        "int c2fbReadStaticBooleans(void) {\n"
        "    return c2fb_enabled * 10 + c2fb_disabled;\n"
        "}\n"
    )
    candidate = source_directory / "static_booleans_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--body-option=--include-global-definitions",
            "--body-option=--include-included-c-source-bodies",
            "--header-compiler-option=-std=gnu2x",
            "--body-compiler-option=-std=gnu2x",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary
    types = candidate.with_name(candidate.stem + "_types.bi").read_text()
    assert "As Byte = 1" in types and "As Byte = 0" in types, types

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu2x", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadStaticBooleans.restype = ctypes.c_int
        expected = reference.c2fbReadStaticBooleans()
        actual = converted.c2fbReadStaticBooleans()
        assert actual == expected == 10, (backend, expected, actual)

    compile_and_compare("c23-static-booleans", source_directory, candidate,
                        reference_path, fbc, check)


def check_pointer_array_assignment(converter, fbc, cc, work):
    source_directory = work / "pointer-array-assignment"
    source_directory.mkdir()
    source = source_directory / "pointer_array.c"
    source.write_text(
        "struct c2fb_node { int value; };\n"
        "static struct c2fb_node *c2fb_nodes[1];\n"
        "void c2fbStoreNode(struct c2fb_node *node) {\n"
        "    c2fb_nodes[0] = node;\n"
        "}\n"
        "int c2fbReadNode(void) { return c2fb_nodes[0]->value; }\n"
    )
    candidate = source_directory / "pointer_array_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        ("--body-option=--include-global-definitions",),
    )
    assert "translated 2 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    class Node(ctypes.Structure):
        _fields_ = [("value", ctypes.c_int)]

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbStoreNode.argtypes = [ctypes.POINTER(Node)]
            library.c2fbReadNode.restype = ctypes.c_int
        for value in (-31, 0, 47):
            node = Node(value)
            reference.c2fbStoreNode(ctypes.byref(node))
            converted.c2fbStoreNode(ctypes.byref(node))
            expected = reference.c2fbReadNode()
            actual = converted.c2fbReadNode()
            assert actual == expected == value, (
                backend, value, expected, actual
            )

    compile_and_compare("pointer-array-assignment", source_directory,
                        candidate, reference_path, fbc, check)


def check_file_scope_allocator_callbacks(converter, fbc, cc, work):
    source_directory = work / "file-scope-allocator-callbacks"
    source_directory.mkdir()
    source = source_directory / "allocator.c"
    source.write_text(
        "#include <stddef.h>\n"
        "#include <stdlib.h>\n"
        "struct allocator {\n"
        "    void *(*allocate)(size_t);\n"
        "    void *(*reallocate)(void *, size_t);\n"
        "    void (*free)(void *);\n"
        "    void (*die)(size_t);\n"
        "};\n"
        "const struct allocator stdlib_allocator = {\n"
        "    malloc, realloc, free, 0\n"
        "};\n"
    )
    candidate = source_directory / "allocator_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        ("--body-option=--include-global-definitions",),
    )
    assert "translated 0 simple functions; skipped 0" in summary, summary
    generated = candidate.read_text()
    for declaration in (
        'Declare Function c2fb_crt_malloc CDecl Alias "malloc"',
        'Declare Function c2fb_crt_realloc CDecl Alias "realloc"',
        'Declare Sub c2fb_crt_free CDecl Alias "free"',
    ):
        assert declaration in generated, generated

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            callbacks = (ctypes.c_void_p * 4).in_dll(
                library, "stdlib_allocator"
            )
            assert callbacks[0] and callbacks[1] and callbacks[2]
            assert callbacks[3] is None
            allocate = ctypes.CFUNCTYPE(
                ctypes.c_void_p, ctypes.c_size_t
            )(callbacks[0])
            reallocate = ctypes.CFUNCTYPE(
                ctypes.c_void_p, ctypes.c_void_p, ctypes.c_size_t
            )(callbacks[1])
            release = ctypes.CFUNCTYPE(None, ctypes.c_void_p)(callbacks[2])
            address = allocate(8)
            assert address
            try:
                ctypes.memmove(address, b"c2fb", 4)
                resized = reallocate(address, 32)
                assert resized
                address = resized
                assert ctypes.string_at(address, 4) == b"c2fb"
            finally:
                release(address)

    compile_and_compare("file-scope-allocator-callbacks", source_directory,
                        candidate, reference_path, fbc, check)


def check_generic_typedef_array_bound(converter, fbc, cc, work):
    source_directory = work / "generic-typedef-array-bound"
    source_directory.mkdir()
    source = source_directory / "generic_bound.c"
    source.write_text(
        "#include <stdint.h>\n"
        "int c2fbReadGenericTypedefArrayBound(void) {\n"
        "    char buffer[_Generic(((__typeof__(intmax_t)){0}),\n"
        "        long int: 21, default: 22)];\n"
        "    return sizeof buffer;\n"
        "}\n"
    )
    candidate = source_directory / "generic_bound_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--header-compiler-option=-std=gnu2x",
            "--body-compiler-option=-std=gnu2x",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu2x", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)
    reference = ctypes.CDLL(str(reference_path))
    reference.c2fbReadGenericTypedefArrayBound.restype = ctypes.c_int
    expected = reference.c2fbReadGenericTypedefArrayBound()
    assert expected > 0

    def check(reference, converted, backend):
        reference.c2fbReadGenericTypedefArrayBound.restype = ctypes.c_int
        converted.c2fbReadGenericTypedefArrayBound.restype = ctypes.c_int
        expected = reference.c2fbReadGenericTypedefArrayBound()
        actual = converted.c2fbReadGenericTypedefArrayBound()
        assert actual == expected, (backend, expected, actual)

    compile_and_compare("generic-typedef-array-bound", source_directory,
                        candidate, reference_path, fbc, check)


def check_generic_enum_constant(converter, fbc, cc, work):
    source_directory = work / "generic-enum-constant"
    source_directory.mkdir()
    source = source_directory / "generic_enum.c"
    source.write_text(
        "#include <stdint.h>\n"
        "enum { c2fb_generic_enum_bound = sizeof(intmax_t) +\n"
        "    _Generic(((__typeof__(intmax_t)){0}),\n"
        "        long int: 17, default: 99) };\n"
        "int c2fbReadGenericEnumBound(void) {\n"
        "    char buffer[c2fb_generic_enum_bound];\n"
        "    return sizeof buffer;\n"
        "}\n"
    )
    candidate = source_directory / "generic_enum_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--header-compiler-option=-std=gnu2x",
            "--body-compiler-option=-std=gnu2x",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu2x", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadGenericEnumBound.restype = ctypes.c_int
        expected = reference.c2fbReadGenericEnumBound()
        actual = converted.c2fbReadGenericEnumBound()
        assert actual == expected == 25, (backend, expected, actual)

    compile_and_compare("generic-enum-constant", source_directory,
                        candidate, reference_path, fbc, check)


def check_scalar_compound_literal_expression(converter, fbc, cc, work):
    source_directory = work / "scalar-compound-literal-expression"
    source_directory.mkdir()
    source = source_directory / "scalar_compound_literal.c"
    source.write_text(
        "#include <stdint.h>\n"
        "uintmax_t c2fbReadScalarCompoundLiteral(\n"
        "    uintmax_t value, uintmax_t divisor) {\n"
        "    return (uintmax_t) { value } % divisor;\n"
        "}\n"
    )
    candidate = source_directory / "scalar_compound_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--header-compiler-option=-std=gnu2x",
            "--body-compiler-option=-std=gnu2x",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu2x", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadScalarCompoundLiteral.argtypes = [
                ctypes.c_ulong, ctypes.c_ulong,
            ]
            library.c2fbReadScalarCompoundLiteral.restype = ctypes.c_ulong
        for value, divisor in ((0, 1), (0xFFFFFFFFFFFF, 97),
                               (123456789, 1000)):
            expected = reference.c2fbReadScalarCompoundLiteral(value, divisor)
            actual = converted.c2fbReadScalarCompoundLiteral(value, divisor)
            assert actual == expected == value % divisor, (
                backend, value, divisor, expected, actual
            )

    compile_and_compare("scalar-compound-literal-expression", source_directory,
                        candidate, reference_path, fbc, check)


def check_local_array_sizeof_bound(converter, fbc, cc, work):
    source_directory = work / "local-array-sizeof-bound"
    source_directory.mkdir()
    source = source_directory / "local_sizeof.c"
    source.write_text(
        "#include <stdint.h>\n"
        "int c2fbReadLocalArraySizeofBound(void) {\n"
        "    char prefix[12];\n"
        "    char buffer[sizeof prefix + sizeof(uintmax_t) +\n"
        "        _Generic(((__typeof__(uintmax_t)){0}),\n"
        "            unsigned long int: 3, default: 4)];\n"
        "    return sizeof buffer;\n"
        "}\n"
    )
    candidate = source_directory / "local_sizeof_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--header-compiler-option=-std=gnu2x",
            "--body-compiler-option=-std=gnu2x",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu2x", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadLocalArraySizeofBound.restype = ctypes.c_int
        expected = reference.c2fbReadLocalArraySizeofBound()
        actual = converted.c2fbReadLocalArraySizeofBound()
        assert actual == expected == 23, (backend, expected, actual)

    compile_and_compare("local-array-sizeof-bound", source_directory,
                        candidate, reference_path, fbc, check)


def check_file_scope_anonymous_enum_object(converter, fbc, cc, work):
    source_directory = work / "file-scope-anonymous-enum-object"
    source_directory.mkdir()
    source = source_directory / "anonymous_enum.c"
    source.write_text(
        "static enum {\n"
        "    C2FB_MODE_DEFAULT = 0,\n"
        "    C2FB_MODE_DOT_AND_DOTDOT,\n"
        "    C2FB_MODE_MINIMAL\n"
        "} c2fb_mode;\n"
        "void c2fbSetAnonymousEnumMode(void) {\n"
        "    c2fb_mode = C2FB_MODE_MINIMAL;\n"
        "}\n"
        "int c2fbReadAnonymousEnumMode(void) { return c2fb_mode; }\n"
    )
    candidate = source_directory / "anonymous_enum_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--body-option=--include-global-definitions",
            "--body-option=--include-included-c-source-bodies",
        ),
    )
    assert "translated 2 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbSetAnonymousEnumMode.restype = None
            library.c2fbReadAnonymousEnumMode.restype = ctypes.c_int
            library.c2fbSetAnonymousEnumMode()
        expected = reference.c2fbReadAnonymousEnumMode()
        actual = converted.c2fbReadAnonymousEnumMode()
        assert actual == expected == 2, (backend, expected, actual)

    compile_and_compare("file-scope-anonymous-enum-object", source_directory,
                        candidate, reference_path, fbc, check)


def check_static_record_array_compound_literals(converter, fbc, cc, work):
    source_directory = work / "static-record-array-compound-literals"
    source_directory.mkdir()
    source = source_directory / "record_array.c"
    source.write_text(
        "#include <stddef.h>\n"
        "struct c2fb_bin_str { size_t length; const char *text; };\n"
        "static struct c2fb_bin_str c2fb_color_table[] = {\n"
        "    { 2, (char const []) { '\\033', '[' } },\n"
        "    { 1, (char const []) { 'm' } },\n"
        "    { 0, (void *) 0 }\n"
        "};\n"
        "int c2fbReadColorByte(int index, int offset) {\n"
        "    if (index < 0 || index >= 3) return -1;\n"
        "    if (offset < 0 || (size_t) offset >= c2fb_color_table[index].length)\n"
        "        return -2;\n"
        "    return (unsigned char) c2fb_color_table[index].text[offset];\n"
        "}\n"
    )
    candidate = source_directory / "record_array_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--body-option=--include-global-definitions",
            "--body-option=--include-included-c-source-bodies",
        ),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadColorByte.argtypes = [ctypes.c_int, ctypes.c_int]
            library.c2fbReadColorByte.restype = ctypes.c_int
        for index, offset in ((0, 0), (0, 1), (1, 0), (2, 0), (3, 0), (0, -1)):
            expected = reference.c2fbReadColorByte(index, offset)
            actual = converted.c2fbReadColorByte(index, offset)
            assert actual == expected, (
                backend, index, offset, expected, actual
            )

    compile_and_compare("static-record-array-compound-literals",
                        source_directory, candidate, reference_path, fbc, check)


def check_multidimensional_array_parameter(converter, fbc, cc, work):
    source_directory = work / "multidimensional-array-parameter"
    source_directory.mkdir()
    source = source_directory / "array_parameter.c"
    source.write_text(
        "unsigned char c2fbStoreAndReadMatrix(\n"
        "    unsigned char matrix[3][5], int row, int column,\n"
        "    unsigned char value) {\n"
        "    matrix[row][column] = value;\n"
        "    return matrix[row][column];\n"
        "}\n"
    )
    candidate = source_directory / "array_parameter_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        ("--header-option=--emit-source=*",),
    )
    assert "translated 1 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbStoreAndReadMatrix.argtypes = [
                ctypes.POINTER(ctypes.c_ubyte), ctypes.c_int, ctypes.c_int,
                ctypes.c_ubyte,
            ]
            library.c2fbStoreAndReadMatrix.restype = ctypes.c_ubyte
        positions = ((0, 0), (1, 4), (2, 3))
        reference_matrix = (ctypes.c_ubyte * 15)()
        converted_matrix = (ctypes.c_ubyte * 15)()
        for value, (row, column) in zip((13, 97, 241), positions):
            expected = reference.c2fbStoreAndReadMatrix(
                reference_matrix, row, column, value
            )
            actual = converted.c2fbStoreAndReadMatrix(
                converted_matrix, row, column, value
            )
            assert actual == expected == value, (
                backend, row, column, expected, actual
            )
            assert converted_matrix[row * 5 + column] == value, (
                backend, row, column, converted_matrix[row * 5 + column]
            )

    compile_and_compare("multidimensional-array-parameter", source_directory,
                        candidate, reference_path, fbc, check)


def check_local_array_passed_to_multidimensional_parameter(
        converter, fbc, cc, work):
    source_directory = work / "local-array-to-multidimensional-parameter"
    source_directory.mkdir()
    source = source_directory / "array_parameter_call.c"
    source.write_text(
        "static unsigned char c2fbReadMatrix(unsigned char matrix[3][5]) {\n"
        "    matrix[1][4] = 73;\n"
        "    return matrix[1][4];\n"
        "}\n"
        "int c2fbCallMatrix(void) {\n"
        "    unsigned char matrix[3][5];\n"
        "    return c2fbReadMatrix(matrix);\n"
        "}\n"
    )
    candidate = source_directory / "array_parameter_call_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        ("--header-option=--emit-source=*",),
    )
    assert "translated 2 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbCallMatrix.restype = ctypes.c_int
        expected = reference.c2fbCallMatrix()
        actual = converted.c2fbCallMatrix()
        assert actual == expected == 73, (backend, expected, actual)

    compile_and_compare("local-array-to-multidimensional-parameter",
                        source_directory, candidate, reference_path, fbc, check)


def check_named_header_inline_body(converter, fbc, cc, work):
    source_directory = work / "named-header-inline-body"
    source_directory.mkdir()
    header = source_directory / "helper.h"
    header.write_text(
        "static inline int c2fbHeaderInlineHelper(int value) {\n"
        "    return value * 3 + 1;\n"
        "}\n"
    )
    source = source_directory / "inline_body.c"
    source.write_text(
        '#include "helper.h"\n'
        "int c2fbReadHeaderInline(int value) {\n"
        "    return c2fbHeaderInlineHelper(value);\n"
        "}\n"
    )
    candidate = source_directory / "inline_body_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
        (
            "--header-option=--emit-source=*",
            "--body-option=--include-header-inline-body=c2fbHeaderInlineHelper",
            "--body-option=--include-global-definitions",
            "--body-option=--include-included-c-source-bodies",
        ),
    )
    assert "translated 2 simple functions; skipped 0" in summary, summary

    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbReadHeaderInline.argtypes = [ctypes.c_int]
            library.c2fbReadHeaderInline.restype = ctypes.c_int
        for value in (-23, 0, 14):
            expected = reference.c2fbReadHeaderInline(value)
            actual = converted.c2fbReadHeaderInline(value)
            assert actual == expected == value * 3 + 1, (
                backend, value, expected, actual
            )

    compile_and_compare("named-header-inline-body", source_directory,
                        candidate, reference_path, fbc, check)


def check_multidimensional_array_storage(converter, fbc, cc, work):
    source_text = (
        "{storage}unsigned char c2fb_matrix[2][3][4];\n"
        "void c2fbWriteMatrix(int i, int j, int k, unsigned char value) {\n"
        "    c2fb_matrix[i][j][k] = value;\n"
        "}\n"
        "unsigned char c2fbReadMatrix(int i, int j, int k) {\n"
        "    return c2fb_matrix[i][j][k];\n"
        "}\n"
        "unsigned char *c2fbMatrixRow(int i, int j) {\n"
        "    return c2fb_matrix[i][j];\n"
        "}\n"
    )

    for storage, body_options in (
        ("", ("--body-option=--include-global-definitions",)),
        ("static ", (
            "--body-option=--include-global-definitions",
            "--body-option=--include-included-c-source-bodies",
        )),
    ):
        name = "body-global-multidimensional" if storage == "" \
            else "header-static-multidimensional"
        source_directory = work / name
        source_directory.mkdir()
        source = source_directory / "matrix.c"
        source.write_text(source_text.replace("{storage}", storage))
        candidate = source_directory / "matrix_candidates.bas"
        summary = translate_source_unit(
            converter, cc, source_directory, source, candidate, body_options,
        )
        assert "translated 3 simple functions; skipped 0" in summary, summary
        reference_path = source_directory / "reference.so"
        run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
                  "-o", reference_path], cwd=source_directory)

        def check(reference, converted, backend):
            for library in (reference, converted):
                library.c2fbWriteMatrix.argtypes = [
                    ctypes.c_int, ctypes.c_int, ctypes.c_int, ctypes.c_ubyte
                ]
                library.c2fbReadMatrix.argtypes = [
                    ctypes.c_int, ctypes.c_int, ctypes.c_int
                ]
                library.c2fbReadMatrix.restype = ctypes.c_ubyte
                library.c2fbMatrixRow.argtypes = [ctypes.c_int, ctypes.c_int]
                library.c2fbMatrixRow.restype = ctypes.POINTER(ctypes.c_ubyte)
            for row in range(2):
                for column in range(3):
                    for item in range(4):
                        value = (row * 37 + column * 11 + item * 3) & 255
                        reference.c2fbWriteMatrix(row, column, item, value)
                        converted.c2fbWriteMatrix(row, column, item, value)
                        assert reference.c2fbReadMatrix(row, column, item) == \
                            converted.c2fbReadMatrix(row, column, item) == value
                    expected_row = bytes(
                        reference.c2fbMatrixRow(row, column)[item]
                        for item in range(4)
                    )
                    actual_row = bytes(
                        converted.c2fbMatrixRow(row, column)[item]
                        for item in range(4)
                    )
                    assert actual_row == expected_row, (backend, row, column)

        compile_and_compare(name, source_directory, candidate, reference_path,
                            fbc, check)

    source_directory = work / "local-multidimensional"
    source_directory.mkdir()
    source = source_directory / "local_matrix.c"
    source.write_text(
        "int c2fbLocalMatrix(int row, int column) {\n"
        "    int values[2][3];\n"
        "    values[row][column] = 42;\n"
        "    return values[row][column];\n"
        "}\n"
    )
    candidate = source_directory / "local_matrix_candidates.bas"
    run([converter, "translate-simple-generic-c", str(source),
         str(candidate), *cc], cwd=source_directory)
    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check_local(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbLocalMatrix.argtypes = [ctypes.c_int, ctypes.c_int]
            library.c2fbLocalMatrix.restype = ctypes.c_int
        for row, column in ((0, 0), (1, 2)):
            assert reference.c2fbLocalMatrix(row, column) == \
                converted.c2fbLocalMatrix(row, column) == 42, (backend, row, column)

    compile_and_compare("local-multidimensional", source_directory,
                        candidate, reference_path, fbc, check_local)


def check_gnu_pointer_alias_statement_expression(converter, fbc, cc, work):
    source_directory = work / "gnu-pointer-alias-statement-expression"
    source_directory.mkdir()
    source = source_directory / "pointer_alias_statement_expression.c"
    source.write_text(
        "#include <stddef.h>\n"
        "struct c2fb_obstack_view { char *next_free; char *object_base; };\n"
        "size_t c2fbObjectSize(struct c2fb_obstack_view *h) {\n"
        "    return __extension__ ({\n"
        "        struct c2fb_obstack_view const *__o = (h);\n"
        "        (size_t) (__o->next_free - __o->object_base);\n"
        "    });\n"
        "}\n"
        "size_t c2fbObjectSizeDivided(struct c2fb_obstack_view *h) {\n"
        "    size_t count;\n"
        "    count = __extension__ ({\n"
        "        struct c2fb_obstack_view const *__o = (h);\n"
        "        (size_t) (__o->next_free - __o->object_base);\n"
        "    }) / 2;\n"
        "    return count;\n"
        "}\n"
    )
    candidate = source_directory / "pointer_alias_statement_expression_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
    )
    assert "translated 2 simple functions" in summary and \
        "skipped 0" in summary, summary
    assert "__extension__" not in candidate.read_text()
    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check_object_size(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbObjectSize.argtypes = [ctypes.c_void_p]
            library.c2fbObjectSize.restype = ctypes.c_size_t
            library.c2fbObjectSizeDivided.argtypes = [ctypes.c_void_p]
            library.c2fbObjectSizeDivided.restype = ctypes.c_size_t
        storage = ctypes.create_string_buffer(64)
        for start, end in ((0, 1), (3, 19), (42, 63)):
            view = (ctypes.c_void_p * 2)(
                ctypes.addressof(storage) + end,
                ctypes.addressof(storage) + start,
            )
            reference_value = reference.c2fbObjectSize(ctypes.byref(view))
            converted_value = converted.c2fbObjectSize(ctypes.byref(view))
            assert reference_value == converted_value == end - start, (
                backend, start, end, reference_value, converted_value)
            reference_divided_value = reference.c2fbObjectSizeDivided(
                ctypes.byref(view))
            divided_value = converted.c2fbObjectSizeDivided(ctypes.byref(view))
            assert reference_divided_value == divided_value == \
                (end - start) // 2, (
                    backend, start, end, reference_divided_value,
                    divided_value)

    compile_and_compare("gnu-pointer-alias-statement-expression",
                        source_directory, candidate, reference_path, fbc,
                        check_object_size)

    side_effect_directory = work / "gnu-statement-expression-side-effect"
    side_effect_directory.mkdir()
    side_effect_source = side_effect_directory / "side_effect.c"
    side_effect_source.write_text(
        "#include <stddef.h>\n"
        "struct c2fb_obstack_view { char *next_free; char *object_base; };\n"
        "extern struct c2fb_obstack_view *c2fbNextView(\n"
        "    struct c2fb_obstack_view *);\n"
        "size_t c2fbObjectSizeWithCall(struct c2fb_obstack_view *h) {\n"
        "    return __extension__ ({\n"
        "        struct c2fb_obstack_view const *__o = c2fbNextView(h);\n"
        "        (size_t) (__o->next_free - __o->object_base);\n"
        "    });\n"
        "}\n"
    )
    side_effect_candidate = side_effect_directory / "side_effect_candidates.bas"
    side_effect_summary = translate_source_unit(
        converter, cc, side_effect_directory, side_effect_source,
        side_effect_candidate,
    )
    assert "translated 0 simple functions; skipped 1" in side_effect_summary, \
        side_effect_summary
    assert "c2fb skipped c2fbObjectSizeWithCall:" in \
        side_effect_candidate.read_text()


def check_unused_local_attribute(converter, fbc, cc, work):
    source_directory = work / "unused-local-attribute"
    source_directory.mkdir()
    source = source_directory / "unused_local_attribute.c"
    source.write_text(
        "int c2fbUnusedLocalAttribute(void) {\n"
        "    __attribute__ ((__unused__)) int local_value = 7;\n"
        "    return 42;\n"
        "}\n"
    )
    candidate = source_directory / "unused_local_attribute_candidates.bas"
    summary = translate_source_unit(
        converter, cc, source_directory, source, candidate,
    )
    assert "translated 1 simple function" in summary and \
        "skipped 0" in summary, summary
    assert "__attribute__" not in candidate.read_text()
    reference_path = source_directory / "reference.so"
    run(cc + ["-std=gnu11", "-shared", "-fPIC", source,
              "-o", reference_path], cwd=source_directory)

    def check(reference, converted, backend):
        for library in (reference, converted):
            library.c2fbUnusedLocalAttribute.argtypes = []
            library.c2fbUnusedLocalAttribute.restype = ctypes.c_int
        assert reference.c2fbUnusedLocalAttribute() == \
            converted.c2fbUnusedLocalAttribute() == 42, backend

    compile_and_compare("unused-local-attribute", source_directory,
                        candidate, reference_path, fbc, check)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    tools_directory = Path(__file__).resolve().parents[1]
    parser.add_argument("--converter", default=str(tools_directory / "build/c2fb"))
    parser.add_argument("--cc", default="cc")
    parser.add_argument("--fbc", default="fbc")
    arguments = parser.parse_args()
    converter = str(Path(arguments.converter).resolve())
    fbc = shlex.split(arguments.fbc)
    cc = shlex.split(arguments.cc)
    with tempfile.TemporaryDirectory(prefix="c2fb-coreutils-regressions-") as temp:
        work = Path(temp)
        check_c23_static_booleans(converter, fbc, cc, work)
        check_pointer_array_assignment(converter, fbc, cc, work)
        check_file_scope_allocator_callbacks(converter, fbc, cc, work)
        check_generic_typedef_array_bound(converter, fbc, cc, work)
        check_generic_enum_constant(converter, fbc, cc, work)
        check_scalar_compound_literal_expression(converter, fbc, cc, work)
        check_local_array_sizeof_bound(converter, fbc, cc, work)
        check_file_scope_anonymous_enum_object(converter, fbc, cc, work)
        check_static_record_array_compound_literals(converter, fbc, cc, work)
        check_multidimensional_array_parameter(converter, fbc, cc, work)
        check_local_array_passed_to_multidimensional_parameter(
            converter, fbc, cc, work)
        check_named_header_inline_body(converter, fbc, cc, work)
        check_multidimensional_array_storage(converter, fbc, cc, work)
        check_gnu_pointer_alias_statement_expression(
            converter, fbc, cc, work)
        check_unused_local_attribute(converter, fbc, cc, work)
    print("All fifteen Coreutils C2FB regressions passed.")


if __name__ == "__main__":
    main()

# end of test_coreutils_c2fb_regressions.py
