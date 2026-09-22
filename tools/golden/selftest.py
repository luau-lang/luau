#!/usr/bin/env python3
"""Self-tests for the tools.golden package."""

from __future__ import annotations

import argparse
import io
import os
import subprocess
import sys
import tempfile
import unittest

from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path
from unittest import mock

from . import cli, execution, suite
from .cli import create_argument_parser, main
from .directives import directives_from_file, parse_directives
from .discovery import discover_tests, select_tests
from .executables import resolve_executables
from .execution import execute_configuration, run_command
from .expectations import (
    remove_actual_outputs,
    update_configuration,
    validate_configuration,
    write_actual_outputs,
)
from .models import (
    COMMANDS,
    CONFIGURATIONS,
    CommandResult,
    ConfigResult,
    DirectiveSpec,
    Executables,
    GoldenError,
    GoldenTest,
    UpdateMode,
)
from .suite import run_suite


class _IntegrationArgs(argparse.Namespace):
    def __init__(self) -> None:
        super().__init__()
        self.luau: str | None = None
        self.luau_analyze: str | None = None


_integration_luau: str | None = None
_integration_analyze: str | None = None


def _write(path: Path, contents: str = "") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as handle:
        _ = handle.write(contents)


def _make_executable(path: Path) -> Path:
    _write(path, "fixture")
    path.chmod(0o755)
    return path


def _directives(text: str = "ok") -> DirectiveSpec:
    return parse_directives([(1, text)], Path("fixture.luau"))


def _test_case(root: Path, directive: str = "ok") -> GoldenTest:
    entry = root / "case.luau"
    _write(entry, f"--!golden {directive}\n")
    return GoldenTest("case", entry, "case.luau", _directives(directive))


def _result(
    strict_output: str = "",
    nonstrict_output: str = "",
    runtime_output: str = "",
    strict_code: int = 0,
    nonstrict_code: int = 0,
    runtime_code: int = 0,
) -> ConfigResult:
    return ConfigResult(
        commands={
            "strict": CommandResult(["strict"], strict_code, strict_output),
            "nonstrict": CommandResult(["nonstrict"], nonstrict_code, nonstrict_output),
            "runtime": CommandResult(["runtime"], runtime_code, runtime_output),
        }
    )


class DirectiveTests(unittest.TestCase):
    def test_bare_directive_has_no_status(self) -> None:
        spec = parse_directives([(1, "")], Path("entry.luau"))

        self.assertIsNone(spec.status_for("flags-on"))
        self.assertIsNone(spec.status_for("flags-off"))

    def test_status_precedence_and_regex_scoping(self) -> None:
        spec = parse_directives(
            [
                (1, "status: ok, flags-off.status: fail"),
                (2, "strict.output: /common/"),
                (3, "flags-on.strict.output: /specific/"),
            ],
            Path("entry.luau"),
        )

        self.assertEqual("ok", spec.status_for("flags-on"))
        self.assertEqual("fail", spec.status_for("flags-off"))
        self.assertEqual(
            ["common", "specific"],
            [item.expression for item in spec.patterns_for("flags-on", "strict")],
        )
        self.assertEqual(
            ["common"],
            [item.expression for item in spec.patterns_for("flags-off", "strict")],
        )

    def test_regex_allows_comma_escaped_slash_and_inline_flags(self) -> None:
        spec = parse_directives(
            [(1, r"status: ok, runtime.output: /(?i)one,two\/three/, nonstrict.output: /done/")],
            Path("entry.luau"),
        )
        pattern = spec.patterns_for("flags-on", "runtime")[0]
        self.assertEqual("(?i)one,two/three", pattern.expression)
        self.assertIsNotNone(pattern.compiled.search("ONE,TWO/THREE"))

    def test_repeated_regexes_are_preserved(self) -> None:
        spec = parse_directives(
            [(1, "status: ok"), (2, "runtime.output: /one/"), (3, "runtime.output: /two/")],
            Path("entry.luau"),
        )
        self.assertEqual(2, len(spec.patterns_for("flags-off", "runtime")))

    def test_fflags_preserve_luau_cli_format(self) -> None:
        value = "DebugLuauUserDefinedClasses,DebugLuauUserDefinedClassesRuntime=true,LuauFlag=false"
        spec = parse_directives([(1, "status: ok"), (2, f"fflags={value}")], Path("entry.luau"))

        self.assertEqual(value, spec.fflags)

    def test_rejects_invalid_directives(self) -> None:
        bad = (
            "status: ok, status: fail",
            "flags-maybe.status: ok",
            "status: maybe",
            "status: ok, strict.stdout: /x/",
            "status: ok, runtime.output: /[/",
            "status: ok, runtime.output: /one/two/",
            "status ok",
            "fflags=",
        )
        for payload in bad:
            with self.subTest(payload=payload), self.assertRaises(GoldenError):
                _ = parse_directives([(1, payload)], Path("entry.luau"))

    def test_rejects_duplicate_fflags(self) -> None:
        with self.assertRaisesRegex(GoldenError, "duplicate fflags"):
            _ = parse_directives(
                [(1, "fflags=DebugLuauFirst"), (2, "fflags=DebugLuauSecond=false")],
                Path("entry.luau"),
            )

    def test_rejects_malformed_directive_markers(self) -> None:
        for directive in ("--!goldenstatus: ok", "--!golden: status: ok", "--!golden-status: ok"):
            with tempfile.TemporaryDirectory() as temporary, self.subTest(directive=directive):
                path = Path(temporary) / "entry.luau"
                _write(path, f"{directive}\n")

                with self.assertRaisesRegex(GoldenError, "malformed golden directive"):
                    _ = directives_from_file(path)


class DiscoveryTests(unittest.TestCase):
    def test_status_is_optional_with_exact_output_for_each_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            # A file with no directive is a test; its output resolves the outcome.
            _write(root / "case.luau", "return true\n")
            _write(root / "case.flags-on.strict.output")

            with self.assertRaisesRegex(GoldenError, "no status resolves.*flags-off"):
                _ = discover_tests(root)

            _write(root / "case.flags-off.nonstrict.output")
            tests = discover_tests(root)

            self.assertEqual(["case"], [test.test_id for test in tests])

    def test_directiveless_standalone_without_output_errors(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "return true\n")
            with self.assertRaisesRegex(GoldenError, "no status resolves"):
                _ = discover_tests(root)

    def test_targeted_update_bootstraps_directiveless_test(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "return true\n")
            update = UpdateMode(
                "targeted",
                frozenset(("strict",)),
                frozenset(("case",)),
                frozenset(CONFIGURATIONS),
            )

            tests = discover_tests(root, update)

            self.assertEqual(["case"], [test.test_id for test in tests])

    def test_targeted_update_does_not_hide_other_incomplete_tests(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "selected.luau", "return true\n")
            _write(root / "unselected.luau", "return true\n")
            update = UpdateMode(
                "targeted",
                frozenset(("strict",)),
                frozenset(("selected",)),
                frozenset(CONFIGURATIONS),
            )

            with self.assertRaisesRegex(GoldenError, "unselected.luau.*no status resolves"):
                _ = discover_tests(root, update)

    def test_targeted_update_must_cover_every_incomplete_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "return true\n")
            update = UpdateMode(
                "targeted",
                frozenset(("strict",)),
                frozenset(("case",)),
                frozenset(("flags-on",)),
            )

            with self.assertRaisesRegex(GoldenError, "no status resolves.*flags-off"):
                _ = discover_tests(root, update)

    def test_rejects_root_level_init(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "init.luau", "--!golden ok\n")
            _write(root / "helper.luau", "return true\n")

            with self.assertRaisesRegex(GoldenError, "root-level init.luau is not supported"):
                _ = discover_tests(root)

    def test_discovers_single_and_multifile_tests(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "single.luau", "--!golden ok\n")
            _write(root / "package" / "init.luau", "--!golden status: ok\n")
            _write(root / "package" / "helper.luau", "return 1\n")
            # A file nested anywhere under an init.luau folder is part of that one
            # test, not a standalone test -- even in a subfolder with no init.luau.
            _write(root / "package" / "sub" / "module.luau", "return 2\n")

            tests = discover_tests(root)

            self.assertEqual(["package", "single"], [test.test_id for test in tests])
            self.assertEqual("package/init.luau", tests[0].entry_argument)
            self.assertEqual(["single"], [test.test_id for test in select_tests(tests, ["single"])])
            with self.assertRaisesRegex(GoldenError, "unknown golden test"):
                _ = select_tests(tests, ["missing"])

    def test_selects_test_groups_transitively(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "analysis" / "tables" / "array.luau", "--!golden ok\n")
            _write(root / "analysis" / "tables" / "nested" / "index.luau", "--!golden ok\n")
            _write(root / "analysis" / "types" / "union.luau", "--!golden ok\n")
            _write(root / "runtime" / "tables.luau", "--!golden ok\n")
            tests = discover_tests(root)

            self.assertEqual(
                ["analysis/tables/array", "analysis/tables/nested/index"],
                [test.test_id for test in select_tests(tests, ["analysis/tables"])],
            )
            self.assertEqual(
                ["analysis/tables/array", "analysis/tables/nested/index", "analysis/types/union"],
                [test.test_id for test in select_tests(tests, ["analysis"])],
            )
            self.assertEqual(
                ["analysis/tables/array", "analysis/tables/nested/index", "analysis/types/union"],
                [test.test_id for test in select_tests(tests, ["analysis", "analysis/tables"])],
            )

            with self.assertRaisesRegex(GoldenError, "unknown golden test ID.*analysis/missing"):
                _ = select_tests(tests, ["analysis/missing"])

    def test_rejects_misplaced_directive_and_absorbs_nested_init(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "late.luau", "local x = 1\n--!golden ok\n")
            with self.assertRaisesRegex(GoldenError, "misplaced"):
                _ = discover_tests(root)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "package" / "init.luau", "--!golden ok\n")
            # A deeper init.luau is a helper of the outer unit, not its own test.
            _write(root / "package" / "nested" / "init.luau", "--!golden ok\n")

            tests = discover_tests(root)

            self.assertEqual(["package"], [test.test_id for test in tests])

    def test_rejects_duplicate_ids_orphan_exact_and_regex_conflict(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "same.luau", "--!golden ok\n")
            _write(root / "same" / "init.luau", "--!golden ok\n")
            with self.assertRaisesRegex(GoldenError, "duplicate golden test ID"):
                _ = discover_tests(root)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "--!golden ok\n")
            _write(root / "missing.flags-on.runtime.output", "orphan\n")
            with self.assertRaisesRegex(GoldenError, "orphan exact-output"):
                _ = discover_tests(root)

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "--!golden status: ok, runtime.output: /value/\n")
            _write(root / "case.flags-on.runtime.output", "value\n")
            with self.assertRaisesRegex(GoldenError, "regex and exact"):
                _ = discover_tests(root)

    def test_rejects_malformed_exact_output_filenames(self) -> None:
        malformed_names = (
            "case.flags-on.strcit.output",
            "case.falgs-on.strict.output",
            "case.strict.output",
            "notes.output",
        )
        for name in malformed_names:
            with tempfile.TemporaryDirectory() as temporary, self.subTest(name=name):
                root = Path(temporary)
                _write(root / "case.luau", "--!golden ok\n")
                _write(root / name)

                with self.assertRaisesRegex(GoldenError, "malformed exact-output filename"):
                    _ = discover_tests(root)

    def test_rejects_empty_suite_and_invalid_exact_utf8(self) -> None:
        with tempfile.TemporaryDirectory() as temporary, self.assertRaisesRegex(GoldenError, "suite is empty"):
            _ = discover_tests(Path(temporary))

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            _write(root / "case.luau", "--!golden ok\n")
            _ = (root / "case.flags-on.runtime.output").write_bytes(b"\xff")
            with self.assertRaisesRegex(GoldenError, "not valid UTF-8"):
                _ = discover_tests(root)


class ExecutableResolutionTests(unittest.TestCase):
    def test_explicit_executable_finds_colocated_counterpart(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            tools = root / "tools"
            luau = _make_executable(tools / "luau")
            analyze = _make_executable(tools / "luau-analyze")

            resolved = resolve_executables(str(luau), None, root, root, {})

            self.assertEqual(luau.resolve(), resolved.luau)
            self.assertEqual(analyze.resolve(), resolved.analyze)

    def test_working_directory_pair_precedes_source_root(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            source = root / "source"
            cwd = root / "cwd"
            source.mkdir()
            cwd.mkdir()
            source_luau = _make_executable(source / "luau")
            _ = _make_executable(source / "luau-analyze")
            cwd_luau = _make_executable(cwd / "luau")
            cwd_analyze = _make_executable(cwd / "luau-analyze")

            resolved = resolve_executables(None, None, source, cwd, {})

            self.assertNotEqual(source_luau.resolve(), resolved.luau)
            self.assertEqual(cwd_luau.resolve(), resolved.luau)
            self.assertEqual(cwd_analyze.resolve(), resolved.analyze)

    def test_ambiguous_build_pairs_include_candidates(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            cwd = root / "cwd"
            source = root / "source"
            cwd.mkdir()
            source.mkdir()
            for name in ("first", "second"):
                _ = _make_executable(source / "build" / name / "luau")
                _ = _make_executable(source / "build" / name / "luau-analyze")

            with self.assertRaisesRegex(GoldenError, "ambiguous Luau build discovery") as raised:
                _ = resolve_executables(None, None, source, cwd, {})
            self.assertIn("first", str(raised.exception))
            self.assertIn("second", str(raised.exception))


class ProcessTests(unittest.TestCase):
    def test_normalizes_output_and_accepts_test_exit_codes(self) -> None:
        completed = subprocess.CompletedProcess([], 1, b"one\r\ntwo\r", None)
        with mock.patch.object(subprocess, "run", return_value=completed) as run:
            result = run_command(["tool"], Path.cwd(), 10)
        self.assertIsNone(result.harness_error)
        self.assertEqual("one\ntwo\n", result.output)
        self.assertEqual(subprocess.PIPE, run.call_args.kwargs["stdout"])
        self.assertEqual(subprocess.STDOUT, run.call_args.kwargs["stderr"])

    def test_classifies_spawn_timeout_signal_exit_and_utf8_failures(self) -> None:
        cases = (
            (OSError("no process"), "could not start"),
            (subprocess.TimeoutExpired(["tool"], 1), "timed out"),
            (subprocess.CompletedProcess([], -9, b"", b""), "signal 9"),
            (subprocess.CompletedProcess([], 7, b"", b""), "unexpected exit code 7"),
            (subprocess.CompletedProcess([], 0, b"\xff", b""), "not valid UTF-8"),
        )
        for outcome, message in cases:
            with self.subTest(message=message):
                patch = (
                    mock.patch.object(subprocess, "run", side_effect=outcome)
                    if isinstance(outcome, BaseException)
                    else mock.patch.object(subprocess, "run", return_value=outcome)
                )
                with patch:
                    result = run_command(["tool"], Path.cwd(), 1)
                self.assertIn(message, result.harness_error or "")

    def test_execute_configuration_uses_required_argv_and_relative_entry(self) -> None:
        test = GoldenTest("nested/case", Path("case.luau"), "nested/case.luau", _directives())
        luau = Path("bin") / "luau"
        analyze = Path("bin") / "luau-analyze"
        executables = Executables(luau, analyze)
        with mock.patch.object(execution, "run_command", return_value=CommandResult(["tool"], 0)) as run:
            _ = execute_configuration(test, "flags-on", executables, Path("tests") / "golden", 3)

        self.assertEqual(
            [
                str(analyze),
                "--mode=strict",
                "--solver=new",
                "--fflags=DebugLuauMagicTypes",
                "--fflags=true",
                "nested/case.luau",
            ],
            run.call_args_list[0].args[0],
        )
        self.assertEqual(
            [
                str(analyze),
                "--mode=nonstrict",
                "--solver=new",
                "--fflags=DebugLuauMagicTypes",
                "--fflags=true",
                "nested/case.luau",
            ],
            run.call_args_list[1].args[0],
        )
        self.assertEqual(
            [str(luau), "--fflags=true", "nested/case.luau"],
            run.call_args_list[2].args[0],
        )

    def test_execute_configuration_orders_runner_and_test_fflags_after_base_configuration(self) -> None:
        runner_fflags = ("DebugLuauFirst", "DebugLuauSecond=false")
        test_fflags = "DebugLuauUserDefinedClasses=true,LuauSomeFlag=false"
        test = GoldenTest("case", Path("case.luau"), "case.luau", _directives(f"fflags={test_fflags}"))
        executables = Executables(Path("luau"), Path("luau-analyze"))

        with mock.patch.object(execution, "run_command", return_value=CommandResult(["tool"], 0)) as run:
            _ = execute_configuration(
                test,
                "flags-off",
                executables,
                Path("tests") / "golden",
                3,
                runner_fflags,
            )

        for call in run.call_args_list:
            argv = call.args[0]
            self.assertEqual("--fflags=false", argv[-5])
            self.assertEqual("--fflags=DebugLuauFirst", argv[-4])
            self.assertEqual("--fflags=DebugLuauSecond=false", argv[-3])
            self.assertEqual(f"--fflags={test_fflags}", argv[-2])
            self.assertEqual("case.luau", argv[-1])


class ValidationAndUpdateTests(unittest.TestCase):
    def test_exact_only_test_does_not_validate_status(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root, "")
            exact_path = test.exact_path("flags-on", "strict")
            test.exact[("flags-on", "strict")] = (exact_path, "exact\n")
            result = _result(strict_output="exact\n", strict_code=1, nonstrict_code=1, runtime_code=1)

            self.assertEqual([], validate_configuration(test, "flags-on", result, None))

    def test_status_exact_and_regex_validation(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root, "status: fail, strict.output: /expected diagnostic/")
            exact_path = test.exact_path("flags-on", "runtime")
            test.exact[("flags-on", "runtime")] = (exact_path, "exact\n")

            passing = _result(strict_output="expected diagnostic\n", runtime_output="exact\n", strict_code=1)
            self.assertEqual([], validate_configuration(test, "flags-on", passing, None))

            failing = _result(strict_output="other\n", runtime_output="wrong\n")
            errors = validate_configuration(test, "flags-on", failing, None)
            self.assertEqual(3, len(errors))
            self.assertTrue(any("expected status fail" in item for item in errors))
            self.assertTrue(any("regex did not match" in item for item in errors))
            self.assertTrue(any("exact output mismatch" in item for item in errors))

    def test_targeted_update_skips_regex_governed_output(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root, "status: ok, strict.output: /keep/")
            update = UpdateMode("targeted", frozenset(("strict",)))
            result = _result(strict_output="keep\n")

            validation = validate_configuration(test, "flags-on", result, update)
            errors, written = update_configuration(test, "flags-on", result, update)

            self.assertEqual([], validation)
            self.assertEqual([], errors)
            self.assertEqual(0, written)
            self.assertFalse(test.exact_path("flags-on", "strict").exists())
            self.assertFalse(test.exact_path("flags-on", "runtime").exists())

    def test_targeted_update_creates_empty_output_file(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root)
            update = UpdateMode("targeted", frozenset(("nonstrict",)))

            errors, written = update_configuration(test, "flags-on", _result(), update)

            self.assertEqual([], errors)
            self.assertEqual(1, written)
            self.assertEqual(b"", test.exact_path("flags-on", "nonstrict").read_bytes())

    def test_update_all_only_rewrites_existing_exact_files(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root)
            existing = test.exact_path("flags-off", "runtime")
            _write(existing, "old\n")
            test.exact[("flags-off", "runtime")] = (existing, "old\n")
            update = UpdateMode("all-existing", frozenset(COMMANDS))
            result = _result(runtime_output="new\n", strict_output="not-created\n")

            self.assertEqual([], validate_configuration(test, "flags-off", result, update))
            errors, written = update_configuration(test, "flags-off", result, update)

            self.assertEqual([], errors)
            self.assertEqual(1, written)
            self.assertEqual("new\n", existing.read_text(encoding="utf-8"))
            self.assertFalse(test.exact_path("flags-off", "strict").exists())

    def test_harness_failure_skips_all_writes_for_configuration(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root)
            result = _result()
            result.commands["strict"].harness_error = "spawn failed"
            update = UpdateMode("targeted", frozenset(COMMANDS))

            errors = validate_configuration(test, "flags-on", result, update)
            update_errors, written = update_configuration(test, "flags-on", result, update)

            self.assertEqual(1, len(errors))
            self.assertEqual([], update_errors)
            self.assertEqual(0, written)
            self.assertEqual([], list(root.glob("*.output")))

    def test_write_actual_dumps_output_and_skips_harness_errors(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = _test_case(root)
            result = _result(strict_output="strict\n", nonstrict_output="nonstrict\n", runtime_output="runtime\n")
            result.commands["runtime"].harness_error = "timed out"

            errors, written = write_actual_outputs(test, "flags-on", result)

            self.assertEqual([], errors)
            self.assertEqual(2, written)
            self.assertEqual("strict\n", test.actual_path("flags-on", "strict").read_text(encoding="utf-8"))
            self.assertEqual("nonstrict\n", test.actual_path("flags-on", "nonstrict").read_text(encoding="utf-8"))
            # A harness failure captured nothing worth dumping.
            self.assertFalse(test.actual_path("flags-on", "runtime").exists())
            # The `.output.tmp` files must not masquerade as exact-output files.
            self.assertEqual(["case"], [found.test_id for found in discover_tests(root)])

    def test_remove_actual_outputs_deletes_only_tmp_files(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            kept = root / "case.flags-on.strict.output"
            _write(kept, "expected\n")
            _write(root / "case.flags-on.strict.output.tmp", "captured\n")
            _write(root / "nested" / "other.flags-off.runtime.output.tmp", "captured\n")

            errors, removed = remove_actual_outputs(root)

            self.assertEqual([], errors)
            self.assertEqual(2, removed)
            self.assertEqual([], list(root.rglob("*.output.tmp")))
            # A committed expectation never carries the `.tmp` suffix, so it survives.
            self.assertTrue(kept.exists())


class RunSuiteTests(unittest.TestCase):
    def test_fail_status_does_not_accept_crashed_command(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            test = GoldenTest("case", root / "case.luau", "case.luau", _directives("fail"))

            completed = subprocess.CompletedProcess([], -11, b"", None)
            with mock.patch.object(subprocess, "run", return_value=completed):
                crashed = run_command(["luau"], root, 10)

            # The ordinary analyzer failure satisfies `--!golden fail`, but the
            # runtime crash must still make the configuration and suite fail.
            result = _result(strict_code=1)
            result.commands["runtime"] = crashed
            buffer = io.StringIO()
            with (
                mock.patch.object(suite, "execute_configuration", return_value=result),
                redirect_stdout(buffer),
                redirect_stderr(buffer),
            ):
                code = run_suite(
                    [test],
                    ["flags-on"],
                    Executables(Path("luau"), Path("luau-analyze")),
                    root,
                    10.0,
                    None,
                    1,
                )

            self.assertEqual(1, code)
            self.assertIn("harness failure for runtime: terminated by signal 11", buffer.getvalue())

    def test_parallel_output_matches_serial(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            first = GoldenTest("first", root / "first.luau", "first.luau", _directives("ok"))
            second = GoldenTest("second", root / "second.luau", "second.luau", _directives("ok"))

            def fake_execute(*_args: object) -> ConfigResult:
                # Status "ok" is expected, so a nonzero code makes every run fail.
                return _result(strict_code=1)

            def run(jobs: int) -> tuple[int, str]:
                buffer = io.StringIO()
                with (
                    mock.patch.object(suite, "execute_configuration", side_effect=fake_execute),
                    redirect_stdout(buffer),
                    redirect_stderr(buffer),
                ):
                    code = run_suite(
                        [first, second],
                        ["flags-on", "flags-off"],
                        Executables(Path("luau"), Path("luau-analyze")),
                        root,
                        10.0,
                        None,
                        jobs,
                    )
                return code, buffer.getvalue()

            serial = run(1)
            self.assertEqual(1, serial[0])
            # Concurrency must not change the outcome or the (sorted) report order.
            self.assertEqual(serial, run(8))


class MainTests(unittest.TestCase):
    def test_help_describes_configurations_and_commands(self) -> None:
        with mock.patch.dict(os.environ, {"PYTHON_COLORS": "0"}):
            help_text = create_argument_parser().format_help()

        self.assertIn(
            """configurations:
  flags-on: run with all CLI fast flags enabled (--fflags=true)
  flags-off: run with all CLI fast flags disabled (--fflags=false)

commands:
  strict: analyze in strict mode with the new solver
  nonstrict: analyze in nonstrict mode with the new solver
  runtime: execute with the Luau interpreter
""",
            help_text,
        )
        self.assertIn("--fflags FLAGS", help_text)

    def test_fflags_option_is_repeatable_and_forwarded(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "--!golden ok\nreturn true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(cli, "run_suite", return_value=0) as run,
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--fflags=DebugLuauFirst", "--fflags=DebugLuauSecond=false"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            self.assertEqual(["DebugLuauFirst", "DebugLuauSecond=false"], run.call_args.kwargs["fflags"])

    def test_rejects_empty_fflags_option(self) -> None:
        with self.assertRaises(SystemExit), redirect_stderr(io.StringIO()):
            _ = main(["--fflags="])

    @unittest.skipUnless(sys.version_info >= (3, 14), "argparse color themes require Python 3.14")
    def test_help_colors_custom_matrix_sections(self) -> None:
        with mock.patch.dict(os.environ, {"PYTHON_COLORS": "1"}):
            help_text = create_argument_parser().format_help()

        self.assertIn("\x1b[1;34mconfigurations:\x1b[0m", help_text)
        self.assertIn("  \x1b[1;32mflags-on\x1b[0m:", help_text)
        self.assertIn("\x1b[1;34mcommands:\x1b[0m", help_text)
        self.assertIn("  \x1b[1;32mstrict\x1b[0m:", help_text)
        self.assertIn("\x1b[1;34mexamples:\x1b[0m", help_text)
        self.assertNotIn("\x1b[1;32mpython3 -m tools.golden\x1b[0m", help_text)

    def test_targeted_update_writes_fresh_test_snapshots(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "return true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="snapshot\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--update=strict", "--jobs=1", "case"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            for config in CONFIGURATIONS:
                self.assertEqual(
                    "snapshot\n",
                    (root / f"case.{config}.strict.output").read_text(encoding="utf-8"),
                )

    def test_targeted_update_bootstraps_fresh_test_group(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "analysis" / "tables" / "array.luau", "return true\n")
            _write(root / "analysis" / "tables" / "nested" / "index.luau", "return true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="snapshot\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--update=strict", "--jobs=1", "analysis/tables"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            for test_id in ("analysis/tables/array", "analysis/tables/nested/index"):
                entry = root / f"{test_id}.luau"
                for config in CONFIGURATIONS:
                    self.assertEqual(
                        "snapshot\n",
                        entry.with_name(f"{entry.stem}.{config}.strict.output").read_text(encoding="utf-8"),
                    )

    def test_targeted_update_accepts_config_list(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "return true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="snapshot\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--update=nonstrict,strict", "--config=flags-off,flags-on", "--jobs=1", "case"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            for config in CONFIGURATIONS:
                for command in ("strict", "nonstrict"):
                    self.assertTrue((root / f"case.{config}.{command}.output").is_file())

    def test_targeted_update_accepts_repeated_configs(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "return true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="snapshot\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    [
                        "--update=strict",
                        "--update=nonstrict",
                        "--config=flags-on",
                        "--config=flags-off",
                        "--jobs=1",
                        "case",
                    ],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            for config in CONFIGURATIONS:
                for command in ("strict", "nonstrict"):
                    self.assertTrue((root / f"case.{config}.{command}.output").is_file())

    def test_update_all_accepts_config_list(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "--!golden ok\nreturn true\n")
            for config in CONFIGURATIONS:
                for command in ("strict", "nonstrict"):
                    _write(root / f"case.{config}.{command}.output", "old\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="new\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--update-all=strict,nonstrict", "--config=flags-on,flags-off", "--jobs=1"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            for config in CONFIGURATIONS:
                for command in ("strict", "nonstrict"):
                    self.assertEqual(
                        "new\n" if command == "strict" else "",
                        (root / f"case.{config}.{command}.output").read_text(encoding="utf-8"),
                    )

    def test_dump_flag_writes_tmp_files_without_affecting_outcome(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "--!golden ok\nreturn true\n")
            executables = Executables(Path("luau"), Path("luau-analyze"))

            with (
                mock.patch.object(cli, "resolve_executables", return_value=executables),
                mock.patch.object(suite, "execute_configuration", return_value=_result(strict_output="captured\n")),
                redirect_stdout(io.StringIO()),
            ):
                code = main(
                    ["--dump", "--jobs=1", "case"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            # A passing comparison run stays passing; the flag only records output.
            self.assertEqual(0, code)
            for config in CONFIGURATIONS:
                self.assertEqual(
                    "captured\n",
                    (root / f"case.{config}.strict.output.tmp").read_text(encoding="utf-8"),
                )
                for command in ("nonstrict", "runtime"):
                    self.assertEqual(
                        "",
                        (root / f"case.{config}.{command}.output.tmp").read_text(encoding="utf-8"),
                    )
            # Nothing exact was written -- only the throwaway files.
            self.assertEqual([], list(root.glob("*.output")))

    def test_clean_removes_tmp_files_without_running(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "case.luau", "--!golden ok\nreturn true\n")
            _write(root / "case.flags-on.strict.output.tmp", "captured\n")
            _write(root / "nested" / "case.flags-off.runtime.output.tmp", "captured\n")

            buffer = io.StringIO()
            # No executables are mocked: --clean must short-circuit before resolving them.
            with redirect_stdout(buffer):
                code = main(
                    ["--clean"],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(0, code)
            self.assertEqual([], list(root.rglob("*.output.tmp")))
            self.assertTrue((root / "case.luau").exists())
            self.assertIn("removed 2 .output.tmp file(s)", buffer.getvalue())

    def test_clean_rejects_test_ids_and_conflicting_modes(self) -> None:
        for argv in (["--clean", "case"], ["--clean", "--dump"], ["--clean", "--update-all=all"]):
            with self.subTest(argv=argv), self.assertRaises(SystemExit), redirect_stderr(io.StringIO()):
                _ = main(
                    argv,
                    source_root=Path("/nonexistent"),
                    test_root=Path("/nonexistent"),
                    cwd=Path("/nonexistent"),
                    environ={},
                )


class EndToEndTests(unittest.TestCase):
    def test_real_clis_apply_runner_fflags_after_each_configuration(self) -> None:
        if not _integration_luau or not _integration_analyze:
            self.skipTest("CLI paths were not supplied")

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(
                root / "classes.luau",
                "--!golden ok\n"
                "class Point\n"
                "    public x: number\n"
                "end\n"
                "local point = Point.new({ x = 42 })\n"
                "assert(point.x == 42)\n",
            )

            code = main(
                [
                    "--luau",
                    _integration_luau,
                    "--luau-analyze",
                    _integration_analyze,
                    "--fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true",
                ],
                source_root=Path(temporary),
                test_root=root,
                cwd=Path(temporary),
                environ={},
            )

            self.assertEqual(0, code)

    def test_real_clis_apply_test_fflags_after_each_configuration(self) -> None:
        if not _integration_luau or not _integration_analyze:
            self.skipTest("CLI paths were not supplied")

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(
                root / "classes.luau",
                "--!golden ok\n"
                "--!golden fflags=DebugLuauUserDefinedClasses=true,DebugLuauUserDefinedClassesRuntime=true\n"
                "class Point\n"
                "    public x: number\n"
                "end\n"
                "local point = Point.new({ x = 42 })\n"
                "assert(point.x == 42)\n",
            )

            code = main(
                ["--luau", _integration_luau, "--luau-analyze", _integration_analyze],
                source_root=Path(temporary),
                test_root=root,
                cwd=Path(temporary),
                environ={},
            )

            self.assertEqual(0, code)

    def test_real_cli_ice_fails_even_with_fail_status(self) -> None:
        if not _integration_luau or not _integration_analyze:
            self.skipTest("CLI paths were not supplied")

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "ice.luau", "--!golden fail\nlocal value: _luau_ice\n")
            buffer = io.StringIO()

            with redirect_stdout(buffer), redirect_stderr(buffer):
                code = main(
                    ["--luau", _integration_luau, "--luau-analyze", _integration_analyze],
                    source_root=Path(temporary),
                    test_root=root,
                    cwd=Path(temporary),
                    environ={},
                )

            self.assertEqual(1, code)
            self.assertIn("unexpected exit code 2", buffer.getvalue())

    def test_real_clis_run_both_configurations(self) -> None:
        if not _integration_luau or not _integration_analyze:
            self.skipTest("CLI paths were not supplied")

        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary) / "tests" / "golden"
            _write(root / "smoke.luau", "--!golden ok\nlocal value: number = 42\nassert(value == 42)\n")

            code = main(
                ["--luau", _integration_luau, "--luau-analyze", _integration_analyze],
                source_root=Path(temporary),
                test_root=root,
                cwd=Path(temporary),
                environ={},
            )

            self.assertEqual(0, code)


def _parse_test_arguments() -> list[str]:
    global _integration_luau, _integration_analyze
    parser = argparse.ArgumentParser(add_help=False)
    _ = parser.add_argument("--luau")
    _ = parser.add_argument("--luau-analyze")
    args, remaining = parser.parse_known_args(namespace=_IntegrationArgs())
    _integration_luau = str(Path(args.luau).resolve()) if args.luau else None
    _integration_analyze = str(Path(args.luau_analyze).resolve()) if args.luau_analyze else None
    return [sys.argv[0], *remaining]


def run_selftests() -> None:
    _ = unittest.main(module=__name__, argv=_parse_test_arguments())


if __name__ == "__main__":
    run_selftests()
