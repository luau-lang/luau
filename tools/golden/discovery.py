"""Golden test discovery, validation, and selection."""

from __future__ import annotations

import re

from collections.abc import Sequence
from pathlib import Path

from .directives import directives_from_file
from .models import COMMANDS, CONFIGURATIONS, GoldenError, GoldenTest, UpdateMode
from .text import read_utf8

_CONFIGURATION_PATTERN = "|".join(re.escape(config) for config in CONFIGURATIONS)
_COMMAND_PATTERN = "|".join(re.escape(command) for command in COMMANDS)
EXACT_NAME_RE = re.compile(
    rf"^(?P<base>.+)\.(?P<config>{_CONFIGURATION_PATTERN})\.(?P<command>{_COMMAND_PATTERN})\.output$"
)


def _owning_init_dir(path: Path, init_dirs: set[Path], test_root: Path) -> Path | None:
    """Outermost ancestor directory (walking test_root -> file) holding an init.luau.

    The shallowest init.luau wins, so a whole subtree forms one multifile test:
    a deeper init.luau is a helper of that unit, not the root of its own.
    Returns None when no ancestor directory holds an init.luau.
    """
    current = test_root
    for part in path.relative_to(test_root).parts[:-1]:
        current = current / part

        if current in init_dirs:
            return current

    return None


def _test_id_matches(test_id: str, selected_id: str) -> bool:
    return test_id == selected_id or test_id.startswith(f"{selected_id}/")


def discover_tests(test_root: Path, update: UpdateMode | None = None) -> list[GoldenTest]:
    if not test_root.is_dir():
        raise GoldenError(f"golden test directory does not exist: {test_root}")

    root_init = test_root / "init.luau"
    if root_init.is_file():
        raise GoldenError(
            f"root-level init.luau is not supported: {root_init}; " + "place multifile tests in a subdirectory"
        )

    luau_files = sorted(test_root.rglob("*.luau"), key=lambda item: item.relative_to(test_root).as_posix())
    init_dirs = {path.parent for path in luau_files if path.name == "init.luau"}

    # Every .luau is a test unless it sits under an init.luau folder, in which
    # case the whole subtree is one multifile test entered through that init.luau.
    entries: list[GoldenTest] = []
    by_id: dict[str, GoldenTest] = {}
    for path in luau_files:
        owner = _owning_init_dir(path, init_dirs, test_root)
        if owner is not None and path != owner / "init.luau":
            continue  # helper within a multifile unit (including any deeper init.luau)

        relative = path.relative_to(test_root)
        test_id = relative.parent.as_posix() if path.name == "init.luau" else relative.with_suffix("").as_posix()
        test = GoldenTest(
            test_id=test_id,
            entry_path=path,
            entry_argument=relative.as_posix(),
            directives=directives_from_file(path),
        )

        if test_id in by_id:
            raise GoldenError(f"duplicate golden test ID {test_id!r}: {by_id[test_id].entry_path} and {path}")

        entries.append(test)
        by_id[test_id] = test

    if not entries:
        raise GoldenError(f"golden suite is empty: {test_root}")

    known_exact: dict[Path, tuple[GoldenTest, str, str]] = {}
    for test in entries:
        for config in CONFIGURATIONS:
            for command in COMMANDS:
                known_exact[test.exact_path(config, command)] = (test, config, command)

    for path in sorted(
        (item for item in test_root.rglob("*.output") if item.is_file()), key=lambda item: item.as_posix()
    ):
        if not EXACT_NAME_RE.match(path.name):
            raise GoldenError(
                f"malformed exact-output filename: {path}; expected "
                + "'<entry>.<configuration>.<command>.output' using "
                + f"configuration(s) {', '.join(CONFIGURATIONS)} and command(s) {', '.join(COMMANDS)}"
            )

        owner = known_exact.get(path)
        if owner is None:
            raise GoldenError(f"orphan exact-output file has no golden entry point: {path}")

        test, config, command = owner
        if test.directives.patterns_for(config, command):
            raise GoldenError(
                f"regex and exact expectations are both present for {test.test_id} {config}.{command}.output: {path}"
            )

        test.exact[(config, command)] = (path, read_utf8(path, "exact-output file"))

    for test in entries:
        for config in CONFIGURATIONS:
            has_exact_output = any((config, command) in test.exact for command in COMMANDS)
            will_create_exact_output = (
                update is not None
                and update.kind == "targeted"
                and any(_test_id_matches(test.test_id, selected_id) for selected_id in update.test_ids)
                and config in update.configs
                and any(
                    command in update.commands and not test.directives.patterns_for(config, command)
                    for command in COMMANDS
                )
            )

            if test.directives.status_for(config) is None and not has_exact_output and not will_create_exact_output:
                raise GoldenError(
                    f"{test.entry_path}: no status resolves for configuration {config} "
                    + "and no exact-output file is present"
                )

    return sorted(entries, key=lambda test: test.test_id)


def select_tests(tests: Sequence[GoldenTest], selected_ids: Sequence[str]) -> list[GoldenTest]:
    if not selected_ids:
        return list(tests)

    unknown = sorted(
        selected_id
        for selected_id in set(selected_ids)
        if not any(_test_id_matches(test.test_id, selected_id) for test in tests)
    )

    if unknown:
        raise GoldenError(f"unknown golden test ID(s): {', '.join(unknown)}")

    return [test for test in tests if any(_test_id_matches(test.test_id, selected_id) for selected_id in selected_ids)]
