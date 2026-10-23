"""Parsing for source-level golden directives."""

from __future__ import annotations

import re

from collections.abc import Sequence
from pathlib import Path

from .models import COMMANDS, CONFIGURATIONS, DirectiveSpec, GoldenError, RegexExpectation
from .text import read_utf8

STATUSES = ("ok", "fail")
DIRECTIVE_RE = re.compile(r"^\s*--!golden(?:\s+(.*?))?\s*$")


def _split_assignments(payload: str, source: str) -> list[str]:
    assignments: list[str] = []
    start = 0
    colon_seen = False
    in_regex = False
    backslashes = 0

    for index, char in enumerate(payload):
        if char == "\\":
            backslashes += 1
            continue

        escaped = backslashes % 2 == 1
        backslashes = 0

        if char == ":" and not colon_seen:
            colon_seen = True
        elif colon_seen and char == "/" and not escaped:
            if not in_regex and payload[start:index].split(":", 1)[1].strip() == "":
                in_regex = True
            elif in_regex:
                in_regex = False
        elif char == "," and not in_regex:
            item = payload[start:index].strip()
            if not item:
                raise GoldenError(f"empty assignment in {source}")
            assignments.append(item)
            start = index + 1
            colon_seen = False

    item = payload[start:].strip()
    if not item:
        raise GoldenError(f"empty assignment in {source}")

    assignments.append(item)
    return assignments


def _decode_regex(expression: str) -> str:
    result: list[str] = []

    index = 0
    while index < len(expression):
        if expression[index : index + 2] == r"\/":
            result.append("/")
            index += 2
        else:
            result.append(expression[index])
            index += 1

    return "".join(result)


def _has_unescaped_slash(expression: str) -> bool:
    backslashes = 0

    for char in expression:
        if char == "\\":
            backslashes += 1
            continue

        if char == "/" and backslashes % 2 == 0:
            return True

        backslashes = 0

    return False


def _parse_assignment(spec: DirectiveSpec, assignment: str, source: str) -> None:
    if ":" not in assignment:
        raise GoldenError(f"expected 'key: value' in {source}: {assignment!r}")

    key, value = (part.strip() for part in assignment.split(":", 1))
    if not key or not value:
        raise GoldenError(f"malformed assignment in {source}: {assignment!r}")

    parts = key.split(".")
    if parts == ["status"]:
        if value not in STATUSES:
            raise GoldenError(f"status must be 'ok' or 'fail' in {source}: {value!r}")

        if spec.global_status is not None:
            raise GoldenError(f"duplicate global status in {source}")

        spec.global_status = value
        return

    if len(parts) == 2 and parts[1] == "status":
        config = parts[0]

        if config not in CONFIGURATIONS:
            raise GoldenError(f"unknown configuration in {source}: {config!r}")

        if value not in STATUSES:
            raise GoldenError(f"status must be 'ok' or 'fail' in {source}: {value!r}")

        if config in spec.config_status:
            raise GoldenError(f"duplicate status for {config} in {source}")

        spec.config_status[config] = value
        return

    config: str | None
    command: str
    if len(parts) == 2:
        config = None
        command, output = parts
    elif len(parts) == 3:
        config, command, output = parts
        if config not in CONFIGURATIONS:
            raise GoldenError(f"unknown configuration in {source}: {config!r}")
    else:
        raise GoldenError(f"unknown directive key in {source}: {key!r}")

    if command not in COMMANDS or output != "output":
        raise GoldenError(f"unknown directive key in {source}: {key!r}")
    if len(value) < 2 or not value.startswith("/") or not value.endswith("/"):
        raise GoldenError(f"regex value must be delimited by '/' in {source}: {value!r}")

    encoded_expression = value[1:-1]
    if _has_unescaped_slash(encoded_expression):
        raise GoldenError(f"literal '/' must be escaped as '\\/' in {source}: {value!r}")
    expression = _decode_regex(encoded_expression)

    try:
        compiled = re.compile(expression)
    except re.error as exc:
        raise GoldenError(f"invalid regex in {source}: {expression!r}: {exc}") from exc
    spec.regexes.setdefault((config, command), []).append(
        RegexExpectation(source=source, expression=expression, compiled=compiled)
    )


def parse_directives(lines: Sequence[tuple[int, str]], path: Path) -> DirectiveSpec:
    spec = DirectiveSpec()
    for line_number, payload in lines:
        source = f"{path}:{line_number}"

        if not payload:
            continue

        if payload.startswith("fflags="):
            fflags = payload.removeprefix("fflags=")
            if not fflags:
                raise GoldenError(f"fflags value must not be empty in {source}")
            if spec.fflags is not None:
                raise GoldenError(f"duplicate fflags in {source}")

            spec.fflags = fflags
            continue

        if payload in STATUSES:
            _parse_assignment(spec, f"status: {payload}", source)
            continue

        for assignment in _split_assignments(payload, source):
            _parse_assignment(spec, assignment, source)

    return spec


def directives_from_file(path: Path) -> DirectiveSpec:
    """Parse the leading ``--!golden`` directives of an entry point.

    Directives are optional metadata; a file with none yields an empty spec, in
    which case status is resolved by exact-output files. Called only on entry
    points, never on helpers.
    """
    text = read_utf8(path, "Luau source")
    directive_lines: list[tuple[int, str]] = []
    content_started = False

    for line_number, line in enumerate(text.split("\n"), 1):
        if match := DIRECTIVE_RE.match(line):
            if content_started:
                raise GoldenError(f"misplaced golden directive at {path}:{line_number}; directives must be leading")
            directive_lines.append((line_number, (match.group(1) or "").strip()))
        elif line.lstrip().startswith("--!golden"):
            raise GoldenError(
                f"malformed golden directive at {path}:{line_number}; "
                + "expected '--!golden' followed by whitespace and directive assignments"
            )
        elif line.strip():
            content_started = True

    return parse_directives(directive_lines, path)
