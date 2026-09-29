"""Shared data models and golden-test matrix definitions."""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, field
from pathlib import Path
from re import Pattern

from typing import Literal


@dataclass(frozen=True)
class Configuration:
    description: str
    fflags: str


@dataclass(frozen=True)
class Command:
    description: str
    executable: Literal["luau", "luau-analyze"]
    arguments: tuple[str, ...]


CONFIGURATIONS: Mapping[str, Configuration] = {
    "flags-on": Configuration(
        description="run with all CLI fast flags enabled (--fflags=true)",
        fflags="true",
    ),
    "flags-off": Configuration(
        description="run with all CLI fast flags disabled (--fflags=false)",
        fflags="false",
    ),
}
COMMANDS: Mapping[str, Command] = {
    "strict": Command(
        description="analyze in strict mode with the new solver",
        executable="luau-analyze",
        arguments=("--mode=strict", "--solver=new", "--fflags=DebugLuauMagicTypes"),
    ),
    "nonstrict": Command(
        description="analyze in nonstrict mode with the new solver",
        executable="luau-analyze",
        arguments=("--mode=nonstrict", "--solver=new", "--fflags=DebugLuauMagicTypes"),
    ),
    "runtime": Command(
        description="execute with the Luau interpreter",
        executable="luau",
        arguments=(),
    ),
}


class GoldenError(Exception):
    """A deterministic suite or command-line validation error."""


@dataclass(frozen=True)
class RegexExpectation:
    source: str
    expression: str
    compiled: Pattern[str]


@dataclass
class DirectiveSpec:
    global_status: str | None = None
    config_status: dict[str, str] = field(default_factory=dict)
    regexes: dict[tuple[str | None, str], list[RegexExpectation]] = field(default_factory=dict)
    fflags: str | None = None

    def status_for(self, config: str) -> str | None:
        return self.config_status.get(config, self.global_status)

    def patterns_for(self, config: str, command: str) -> list[RegexExpectation]:
        return self.regexes.get((None, command), []) + self.regexes.get((config, command), [])


@dataclass
class GoldenTest:
    test_id: str
    entry_path: Path
    entry_argument: str
    directives: DirectiveSpec
    exact: dict[tuple[str, str], tuple[Path, str]] = field(default_factory=dict)

    def exact_path(self, config: str, command: str) -> Path:
        return self.entry_path.with_name(f"{self.entry_path.stem}.{config}.{command}.output")

    def actual_path(self, config: str, command: str) -> Path:
        # Captured output for a run, written beside (and sorting next to) the
        # expected `.output` so the two can be diffed directly. The `.tmp`
        # suffix keeps discovery's `*.output` glob from picking these up and is
        # gitignored so throwaway captures cannot be committed.
        exact = self.exact_path(config, command)
        return exact.with_name(f"{exact.name}.tmp")


@dataclass
class CommandResult:
    argv: list[str]
    returncode: int | None = None
    output: str = ""
    harness_error: str | None = None


@dataclass
class ConfigResult:
    commands: dict[str, CommandResult]

    @property
    def has_harness_error(self) -> bool:
        return any(result.harness_error is not None for result in self.commands.values())


@dataclass(frozen=True)
class Executables:
    luau: Path
    analyze: Path


@dataclass(frozen=True)
class UpdateMode:
    kind: str  # "targeted" or "all-existing"
    commands: frozenset[str]
    test_ids: frozenset[str] = frozenset()
    configs: frozenset[str] = frozenset()
