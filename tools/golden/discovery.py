"""Golden test discovery, validation, and selection."""

from __future__ import annotations

import re

from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from .directives import directives_from_file
from .models import COMMANDS, CONFIGURATIONS, GoldenError, GoldenTest, UpdateMode
from .text import read_utf8

_CONFIGURATION_PATTERN = "|".join(re.escape(config) for config in CONFIGURATIONS)
_COMMAND_PATTERN = "|".join(re.escape(command) for command in COMMANDS)
EXACT_NAME_RE = re.compile(
    rf"^(?P<base>.+)\.(?P<config>{_CONFIGURATION_PATTERN})\.(?P<command>{_COMMAND_PATTERN})\.output$"
)


@dataclass(frozen=True)
class GoldenTestEntry:
    test_id: str
    entry_path: Path


@dataclass(frozen=True)
class GoldenTestIndex:
    test_root: Path
    entries: tuple[GoldenTestEntry, ...]


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
    return selected_id == "." or test_id == selected_id or test_id.startswith(f"{selected_id}/")


def _validate_test_selectors(test_ids: Sequence[str], selected_ids: Sequence[str]) -> None:
    unknown = sorted(
        selected_id
        for selected_id in set(selected_ids)
        if not any(_test_id_matches(test_id, selected_id) for test_id in test_ids)
    )

    if unknown:
        raise GoldenError(f"unknown golden test ID(s) or path(s): {', '.join(unknown)}")


def _is_explicit_path(selector: str, path: Path) -> bool:
    return path.is_absolute() or path.suffix == ".luau" or selector.startswith(("./", "../", ".\\", "..\\", "~"))


def _resolve_selector_path(selector: str, test_root: Path, cwd: Path) -> Path | None:
    path = Path(selector).expanduser()
    explicit_path = _is_explicit_path(selector, path)

    if path.is_absolute():
        candidate = path.resolve()
    else:
        cwd_candidate = (cwd / path).resolve()
        if cwd_candidate.exists() and (explicit_path or cwd_candidate.is_relative_to(test_root)):
            candidate = cwd_candidate
        else:
            candidate = (test_root / path).resolve()

    if not candidate.exists():
        return None

    if not candidate.is_relative_to(test_root):
        if explicit_path:
            raise GoldenError(f"golden test path is outside the test root: {selector}")
        return None

    return candidate


def _test_id_for_path(path: Path, index: GoldenTestIndex) -> str:
    if path.is_file() and path.suffix != ".luau":
        raise GoldenError(f"golden test path is not a Luau source file or directory: {path}")

    for entry in index.entries:
        if entry.entry_path.name == "init.luau" and path.is_relative_to(entry.entry_path.parent):
            return entry.test_id

    relative = path.relative_to(index.test_root)
    if path.is_dir():
        return relative.as_posix()

    return relative.with_suffix("").as_posix()


def _normalize_test_selectors(
    selectors: Sequence[str],
    index: GoldenTestIndex,
    cwd: Path,
) -> list[str]:
    """Convert source-file and directory paths to canonical golden test IDs."""
    resolved_cwd = cwd.resolve()

    normalized: list[str] = []
    for selector in selectors:
        path = _resolve_selector_path(selector, index.test_root, resolved_cwd)
        normalized.append(_test_id_for_path(path, index) if path is not None else selector)

    return normalized


def discover_test_index(test_root: Path) -> GoldenTestIndex:
    resolved_test_root = test_root.resolve()
    if not resolved_test_root.is_dir():
        raise GoldenError(f"golden test directory does not exist: {resolved_test_root}")

    luau_files = sorted(
        resolved_test_root.rglob("*.luau"), key=lambda item: item.relative_to(resolved_test_root).as_posix()
    )
    init_dirs = {path.parent for path in luau_files if path.name == "init.luau"}

    # Every .luau is a test unless it sits under an init.luau folder, in which
    # case the whole subtree is one multifile test entered through that init.luau.
    entries: list[GoldenTestEntry] = []
    for path in luau_files:
        owner = _owning_init_dir(path, init_dirs, resolved_test_root)
        if owner is not None and path != owner / "init.luau":
            continue  # helper within a multifile unit (including any deeper init.luau)

        relative = path.relative_to(resolved_test_root)
        test_id = relative.parent.as_posix() if path.name == "init.luau" else relative.with_suffix("").as_posix()
        entries.append(GoldenTestEntry(test_id, path))

    return GoldenTestIndex(resolved_test_root, tuple(entries))


def select_test_entries(index: GoldenTestIndex, selectors: Sequence[str], cwd: Path) -> list[GoldenTestEntry]:
    if not selectors:
        return list(index.entries)

    selected_ids = _normalize_test_selectors(selectors, index, cwd)
    _validate_test_selectors([entry.test_id for entry in index.entries], selected_ids)

    return [
        entry
        for entry in index.entries
        if any(_test_id_matches(entry.test_id, selected_id) for selected_id in selected_ids)
    ]


def load_and_validate_tests(index: GoldenTestIndex, update: UpdateMode | None = None) -> list[GoldenTest]:
    root_init = index.test_root / "init.luau"
    if root_init.is_file():
        raise GoldenError(
            f"root-level init.luau is not supported: {root_init}; " + "place multifile tests in a subdirectory"
        )

    entries_by_id: dict[str, GoldenTestEntry] = {}
    for entry in index.entries:
        previous = entries_by_id.get(entry.test_id)
        if previous is not None:
            raise GoldenError(
                f"duplicate golden test ID {entry.test_id!r}: {previous.entry_path} and {entry.entry_path}"
            )
        entries_by_id[entry.test_id] = entry

    if not index.entries:
        raise GoldenError(f"golden suite is empty: {index.test_root}")

    tests = [
        GoldenTest(
            test_id=entry.test_id,
            entry_path=entry.entry_path,
            entry_argument=entry.entry_path.relative_to(index.test_root).as_posix(),
            directives=directives_from_file(entry.entry_path),
        )
        for entry in index.entries
    ]

    known_exact: dict[Path, tuple[GoldenTest, str, str]] = {}
    for test in tests:
        for config in CONFIGURATIONS:
            for command in COMMANDS:
                known_exact[test.exact_path(config, command)] = (test, config, command)

    for path in sorted(
        (item for item in index.test_root.rglob("*.output") if item.is_file()), key=lambda item: item.as_posix()
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

    for test in tests:
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

    return sorted(tests, key=lambda test: test.test_id)
