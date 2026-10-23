"""Process execution for each golden-test matrix entry."""

from __future__ import annotations

import subprocess

from collections.abc import Sequence
from pathlib import Path

from .models import COMMANDS, CONFIGURATIONS, CommandResult, ConfigResult, Executables, GoldenTest
from .text import normalize_newlines


def run_command(argv: Sequence[str], cwd: Path, timeout: float) -> CommandResult:
    rendered = [str(item) for item in argv]
    try:
        completed = subprocess.run(
            rendered,
            cwd=str(cwd),
            stdin=subprocess.DEVNULL,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            timeout=timeout,
            shell=False,
            check=False,
        )
    except subprocess.TimeoutExpired:
        return CommandResult(argv=rendered, harness_error=f"timed out after {timeout:g} seconds")
    except OSError as exc:
        return CommandResult(argv=rendered, harness_error=f"could not start process: {exc}")

    result = CommandResult(argv=rendered, returncode=completed.returncode)

    if completed.returncode < 0:
        result.harness_error = f"terminated by signal {-completed.returncode}"
        return result

    if completed.returncode not in (0, 1):
        result.harness_error = f"unexpected exit code {completed.returncode}; expected 0 or 1"
        return result

    try:
        result.output = normalize_newlines(completed.stdout.decode("utf-8"))
    except UnicodeDecodeError as exc:
        result.harness_error = f"output is not valid UTF-8: {exc}"

    return result


def execute_configuration(
    test: GoldenTest,
    config: str,
    executables: Executables,
    test_root: Path,
    timeout: float,
    fflags: Sequence[str] = (),
) -> ConfigResult:
    executable_paths = {
        "luau": executables.luau,
        "luau-analyze": executables.analyze,
    }
    configuration = CONFIGURATIONS[config]
    results: dict[str, CommandResult] = {}

    for command, command_spec in COMMANDS.items():
        argv = [str(executable_paths[command_spec.executable])]
        argv.extend(command_spec.arguments)
        argv.append(f"--fflags={configuration.fflags}")
        argv.extend(f"--fflags={value}" for value in fflags)
        if test.directives.fflags is not None:
            argv.append(f"--fflags={test.directives.fflags}")
        argv.append(test.entry_argument)
        results[command] = run_command(argv, test_root, timeout)

    return ConfigResult(commands=results)
