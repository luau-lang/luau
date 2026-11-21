"""Output validation, exact-snapshot reading and writing, and snapshot updates."""

from __future__ import annotations

import os
import re
import shlex
import stat
import tempfile
import tomllib

from collections.abc import Mapping
from contextlib import suppress
from pathlib import Path

from .models import COMMANDS, CONFIGURATIONS, ConfigResult, GoldenError, GoldenTest, UpdateMode
from .text import read_utf8, unified_diff

# TOML multiline literal strings cannot contain `'''` or control characters other than tab and newline.
_LITERAL_FORBIDDEN_RE = re.compile(r"[\x00-\x08\x0b-\x1f\x7f]|'''")
_BASIC_ESCAPES = {"\\": "\\\\", '"': '\\"', "\n": "\\n", "\t": "\\t"}


def read_snapshot(path: Path) -> dict[tuple[str, str], str]:
    """Parse a snapshot into `(config, command) -> exact output`.

    A missing key means that command's output is unconstrained; an empty string
    requires empty output. Raises `GoldenError` for invalid TOML or any table,
    key, or value outside the golden matrix.
    """
    try:
        document = tomllib.loads(read_utf8(path, "golden snapshot"))
    except tomllib.TOMLDecodeError as exc:
        raise GoldenError(f"invalid TOML in golden snapshot {path}: {exc}") from exc

    outputs: dict[tuple[str, str], str] = {}
    for config, table in document.items():
        if config not in CONFIGURATIONS:
            raise GoldenError(
                f"unknown configuration {config!r} in golden snapshot {path}; "
                + f"expected one of {', '.join(CONFIGURATIONS)}"
            )
        if not isinstance(table, dict):
            raise GoldenError(f"[{config}] must be a table in golden snapshot {path}")

        for command, output in table.items():
            if command not in COMMANDS:
                raise GoldenError(
                    f"unknown command {command!r} in [{config}] of golden snapshot {path}; "
                    + f"expected one of {', '.join(COMMANDS)}"
                )
            if not isinstance(output, str):
                raise GoldenError(f"{config}.{command} must be a string in golden snapshot {path}")
            outputs[(config, command)] = output

    return outputs


def _format_string(text: str) -> str:
    if not text:
        return '""'

    # A multiline literal keeps captured output byte-for-byte readable. TOML trims
    # the newline right after the opening delimiter, and permits up to two quotes
    # immediately before the closing one.
    if _LITERAL_FORBIDDEN_RE.search(text) is None:
        return f"'''\n{text}'''"

    escaped = "".join(
        _BASIC_ESCAPES.get(char) or (f"\\u{ord(char):04x}" if char < " " or char == "\x7f" else char) for char in text
    )
    return f'"{escaped}"'


def format_snapshot(outputs: Mapping[tuple[str, str], str]) -> str:
    """Serialize outputs deterministically in matrix order, omitting empty tables."""
    tables: list[str] = []
    for config in CONFIGURATIONS:
        lines = [
            f"{command} = {_format_string(outputs[(config, command)])}"
            for command in COMMANDS
            if (config, command) in outputs
        ]
        if lines:
            tables.append(f"[{config}]\n" + "\n".join(lines) + "\n")

    return "\n".join(tables)


def _captured_output(command: str, text: str) -> str:
    if not text:
        return f"captured {command}.output: <empty>"
    return f"captured {command}.output:\n---\n{text}\n---"


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
    *,
    color: bool = False,
) -> list[str]:
    errors: list[str] = []
    for command, command_result in result.commands.items():
        if command_result.harness_error:
            message = f"{test.test_id} [{config}] harness failure for {command}: "
            message += f"{command_result.harness_error}\nargv: {shlex.join(command_result.argv)}"
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

        expected = test.exact.get((config, command))
        if expected is not None and expected != actual and not _will_update(test, config, command, update):
            message = f"{test.test_id} [{config}] exact output mismatch for {command}.output:\n"
            message += unified_diff(f"{test.snapshot_path} [{config}.{command}]", expected, actual, color=color)
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


def write_actual_outputs(test: GoldenTest, results: Mapping[str, ConfigResult]) -> str | None:
    """Dump every command's captured output across `results` to the test's `.golden.toml.tmp`.

    Independent of validation and updates: it records what each run produced, in
    the snapshot format, so a failure can be inspected or diffed against the
    expected `.golden.toml`. A command with a harness error captured no usable
    output, so it is omitted. Returns an error message if the write fails.
    """
    outputs = {
        (config, command): command_result.output
        for config, result in results.items()
        for command, command_result in result.commands.items()
        if command_result.harness_error is None
    }

    try:
        _atomic_write(test.actual_path, format_snapshot(outputs))
    except (OSError, UnicodeError) as exc:
        return f"{test.test_id} could not write {test.actual_path}: {exc}"

    return None


def remove_actual_outputs(test_root: Path) -> tuple[list[str], int]:
    """Delete every `.golden.toml.tmp` capture beneath `test_root`.

    The inverse of `write_actual_outputs`: it clears the throwaway files a
    `--dump` run leaves behind. Committed `.golden.toml` expectations are
    untouched because they never carry the `.tmp` suffix.
    """
    errors: list[str] = []
    removed = 0
    for path in sorted(test_root.rglob("*.golden.toml.tmp"), key=lambda item: item.as_posix()):
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
) -> int:
    """Record the captured outputs `update` selects into `test.exact`, returning how many were updated.

    Only updates the in-memory expectations; `write_snapshot` persists them. A
    configuration with a harness error keeps its previous expectations.
    """
    if result.has_harness_error:
        return 0

    updated = 0
    for command in COMMANDS:
        if _will_update(test, config, command, update):
            test.exact[(config, command)] = result.commands[command].output
            updated += 1

    return updated


def write_snapshot(test: GoldenTest) -> str | None:
    """Write `test.exact` to the test's `.golden.toml`, returning an error message if the write fails."""
    try:
        _atomic_write(test.snapshot_path, format_snapshot(test.exact))
    except (OSError, UnicodeError) as exc:
        return f"{test.test_id} could not update {test.snapshot_path}: {exc}"

    return None
