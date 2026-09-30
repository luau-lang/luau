"""Output validation and exact-snapshot updates."""

from __future__ import annotations

import difflib
import os
import stat
import tempfile

from contextlib import suppress
from pathlib import Path

from .models import COMMANDS, ConfigResult, GoldenTest, UpdateMode


def _captured_output(command: str, text: str) -> str:
    if not text:
        return f"captured {command}.output: <empty>"
    return f"captured {command}.output:\n---\n{text}\n---"


def _unified_diff(expected_path: Path, expected: str, actual: str) -> str:
    diff = "".join(
        difflib.unified_diff(
            expected.splitlines(keepends=True),
            actual.splitlines(keepends=True),
            fromfile=str(expected_path),
            tofile="captured output",
        )
    )
    return diff or f"expected {expected!r}, captured {actual!r}"


def _will_update(
    test: GoldenTest,
    config: str,
    command: str,
    update: UpdateMode | None,
) -> bool:
    if update is None or command not in update.commands:
        return False

    if test.directives.patterns_for(config, command):
        return False

    if update.kind == "targeted":
        return True

    return (config, command) in test.exact


def validate_configuration(
    test: GoldenTest,
    config: str,
    result: ConfigResult,
    update: UpdateMode | None,
) -> list[str]:
    errors: list[str] = []
    for command, command_result in result.commands.items():
        if command_result.harness_error:
            message = f"{test.test_id} [{config}] harness failure for {command}: "
            message += f"{command_result.harness_error}\nargv: {command_result.argv!r}"
            errors.append(message)

    if result.has_harness_error:
        return errors

    expected_status = test.directives.status_for(config)
    if expected_status is not None:
        returncodes = {command: result.commands[command].returncode for command in COMMANDS}
        status_matches = (
            all(code == 0 for code in returncodes.values())
            if expected_status == "ok"
            else any(code == 1 for code in returncodes.values())
        )
        if not status_matches:
            captured = "\n".join(_captured_output(command, result.commands[command].output) for command in COMMANDS)
            message = f"{test.test_id} [{config}] expected status {expected_status}, "
            actual_statuses = " ".join(f"{command}={returncodes[command]}" for command in COMMANDS)
            message += f"got {actual_statuses}\n{captured}"
            errors.append(message)

    for command in COMMANDS:
        command_result = result.commands[command]
        actual = command_result.output
        patterns = test.directives.patterns_for(config, command)
        for expectation in patterns:
            if expectation.compiled.search(actual) is None:
                message = f"{test.test_id} [{config}] regex did not match {command}.output: "
                message += f"/{expectation.expression}/ ({expectation.source})\n"
                message += _captured_output(command, actual)
                errors.append(message)

        exact = test.exact.get((config, command))
        if exact and not _will_update(test, config, command, update):
            path, expected = exact
            if expected != actual:
                message = f"{test.test_id} [{config}] exact output mismatch for {command}.output:\n"
                message += _unified_diff(path, expected, actual)
                errors.append(message)

    return errors


def _atomic_write(path: Path, text: str) -> None:
    # Write to a temporary file in the same directory, then rename over the target.
    # This allows us to retain atomic behavior in the face of interruptions to file writes.
    path.parent.mkdir(parents=True, exist_ok=True)
    old_mode = stat.S_IMODE(path.stat().st_mode) if path.exists() else 0o644
    temporary_name: str | None = None

    try:
        with tempfile.NamedTemporaryFile("wb", dir=str(path.parent), prefix=f".{path.name}.", delete=False) as handle:
            temporary_name = handle.name
            _ = handle.write(text.encode("utf-8"))
            handle.flush()
            os.fsync(handle.fileno())

        # `NamedTemporaryFile` is created with 0o600, so we have to chmod back to the original permissions
        os.chmod(temporary_name, old_mode)
        os.replace(temporary_name, path)
        temporary_name = None
    finally:
        if temporary_name is not None:
            with suppress(FileNotFoundError):
                os.unlink(temporary_name)


def write_actual_outputs(
    test: GoldenTest,
    config: str,
    result: ConfigResult,
) -> tuple[list[str], int]:
    """Dump every command's captured output to its `.output.tmp` file.

    Independent of validation and updates: it records what each run produced so
    a failure can be inspected or diffed against the expected `.output`. A
    command with a harness error captured no usable output, so it is skipped.
    """
    errors: list[str] = []
    written = 0
    for command in COMMANDS:
        command_result = result.commands[command]
        if command_result.harness_error is not None:
            continue

        path = test.actual_path(config, command)
        try:
            _atomic_write(path, command_result.output)
            written += 1
        except (OSError, UnicodeError) as exc:
            errors.append(f"{test.test_id} [{config}] could not write {path}: {exc}")

    return errors, written


def remove_actual_outputs(test_root: Path) -> tuple[list[str], int]:
    """Delete every `.output.tmp` capture beneath `test_root`.

    The inverse of `write_actual_outputs`: it clears the throwaway files a
    `--dump` run leaves behind. Committed `.output` expectations are untouched
    because they never carry the `.tmp` suffix.
    """
    errors: list[str] = []
    removed = 0
    for path in sorted(test_root.rglob("*.output.tmp"), key=lambda item: item.as_posix()):
        if not path.is_file():
            continue
        try:
            path.unlink()
            removed += 1
        except OSError as exc:
            errors.append(f"could not remove {path}: {exc}")

    return errors, removed


def update_configuration(
    test: GoldenTest,
    config: str,
    result: ConfigResult,
    update: UpdateMode,
) -> tuple[list[str], int]:
    if result.has_harness_error:
        return [], 0

    errors: list[str] = []
    written = 0
    for command in COMMANDS:
        if not _will_update(test, config, command, update):
            continue

        path = test.exact_path(config, command)
        try:
            _atomic_write(path, result.commands[command].output)
            written += 1
        except (OSError, UnicodeError) as exc:
            errors.append(f"{test.test_id} [{config}] could not update {path}: {exc}")

    return errors, written
