"""Command-line interface for the Luau golden test runner."""

from __future__ import annotations

import argparse
import os
import sys

from collections.abc import Callable, Mapping, Sequence
from pathlib import Path

from .discovery import (
    discover_test_index,
    load_and_validate_tests,
    select_test_entries,
)
from .executables import resolve_executables
from .expectations import remove_actual_outputs
from .models import COMMANDS, CONFIGURATIONS, GoldenError, UpdateMode
from .suite import run_suite

from typing import TYPE_CHECKING, Protocol, TypeVar, cast

if TYPE_CHECKING:
    from typing import override
else:
    _CallableT = TypeVar("_CallableT", bound=Callable[..., object])

    def override(method: _CallableT, /) -> _CallableT:
        return method


CUSTOM_HELP_HEADINGS = ("configurations:", "commands:", "examples:")


class _HelpTheme(Protocol):
    heading: str
    action: str
    reset: str


class ParsedArguments(argparse.Namespace):
    def __init__(self) -> None:
        super().__init__()
        self.test_ids: list[str] = []
        self.luau: str | None = None
        self.luau_analyze: str | None = None
        self.fflags: list[str] = []
        self.update: list[str] = []
        self.update_all: list[str] = []
        self.config: list[str] = []
        self.timeout: float = 10.0
        self.test_root: str | None = None
        self.jobs: int = 0
        self.dump: bool = False
        self.clean: bool = False


class GoldenHelpFormatter(argparse.RawDescriptionHelpFormatter):
    @override
    def _format_text(self, text: str) -> str:
        theme = cast(_HelpTheme | None, getattr(self, "_theme", None))

        if theme is not None:
            heading_color = theme.heading
            item_color = theme.action
            reset = theme.reset
            colored_lines: list[str] = []
            in_matrix_section = False

            for line in text.splitlines(keepends=True):
                content = line.rstrip("\r\n")
                ending = line[len(content) :]

                if content in CUSTOM_HELP_HEADINGS:
                    in_matrix_section = content != "examples:"
                    content = f"{heading_color}{content}{reset}"
                elif in_matrix_section and content.startswith("  ") and ":" in content:
                    name, description = content[2:].split(":", 1)
                    content = f"  {item_color}{name}{reset}:{description}"
                colored_lines.append(content + ending)

            text = "".join(colored_lines)

        return super()._format_text(text)


def _selected_matrix_values(
    values: Sequence[str],
    allowed: Sequence[str],
    option: str,
    parser: argparse.ArgumentParser,
) -> list[str]:
    selected: list[str] = []
    for value in values:
        for item in value.split(","):
            item = item.strip()

            if not item:
                parser.error(f"{option} contains an empty value")

            if item != "all" and item not in allowed:
                parser.error(f"unknown {option} value: {item!r}")

            if item not in selected:
                selected.append(item)

    if not selected or selected == ["all"]:
        return list(allowed)

    if "all" in selected:
        parser.error(f"{option} 'all' cannot be combined with named values")

    return selected


def _matrix_help() -> str:
    lines = ["configurations:"]
    lines.extend(f"  {config}: {spec.description}" for config, spec in CONFIGURATIONS.items())
    lines.extend(("", "commands:"))
    lines.extend(f"  {command}: {spec.description}" for command, spec in COMMANDS.items())
    return "\n".join(lines)


def create_argument_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        description="Run the Luau golden file testing framework.",
        epilog=(
            f"{_matrix_help()}\n\n"
            "examples:\n"
            "  python3 -m tools.golden\n"
            "  python3 -m tools.golden types/generic runtime/assertions\n"
            "  python3 -m tools.golden tests/golden/types/generic.luau\n"
            "  python3 -m tools.golden --fflags=DebugLuauUserDefinedClasses=true\n"
            "  python3 -m tools.golden --update=strict --config=flags-on types/generic\n"
            "  python3 -m tools.golden --update-all=all --config=all\n"
            "  python3 -m tools.golden --dump types/generic\n"
            "  python3 -m tools.golden --clean\n\n"
            "See tools/golden/golden.md for more detailed documentation."
        ),
        formatter_class=GoldenHelpFormatter,
    )
    _ = parser.add_argument(
        "test_ids",
        nargs="*",
        metavar="TEST",
        help="test or directory ID, or a Luau source file or directory path",
    )
    _ = parser.add_argument("--luau", metavar="PATH", help="path to the luau executable")
    _ = parser.add_argument("--luau-analyze", metavar="PATH", help="path to the luau-analyze executable")
    _ = parser.add_argument(
        "--fflags",
        action="append",
        default=[],
        metavar="FLAGS",
        help="fast flags forwarded to every Luau command; may be repeated",
    )
    update_group = parser.add_mutually_exclusive_group()
    _ = update_group.add_argument(
        "--update",
        action="append",
        default=[],
        metavar="COMMAND[,COMMAND...]",
        help="rewrite selected command(s); may be comma-separated or repeated",
    )
    _ = update_group.add_argument(
        "--update-all",
        action="append",
        default=[],
        metavar="COMMAND[,COMMAND...]",
        help="rewrite existing exact files for selected command(s); may be comma-separated or repeated",
    )
    _ = parser.add_argument(
        "--config",
        action="append",
        default=[],
        metavar="CONFIG[,CONFIG...]",
        help="configuration(s) to update; may be comma-separated or repeated (default: all)",
    )
    _ = parser.add_argument("--timeout", type=float, default=10.0, metavar="SECONDS")
    _ = parser.add_argument(
        "--test-root",
        metavar="PATH",
        help="directory holding the golden corpus (default: the tests/golden beside this tool)",
    )
    _ = parser.add_argument(
        "--jobs",
        type=int,
        default=0,
        metavar="N",
        help="concurrent CLI runs; 0 (default) auto-sizes to the machine, 1 forces serial",
    )
    _ = parser.add_argument(
        "--dump",
        action="store_true",
        help="dump each run's captured output to a gitignored '<entry>.<config>.<command>.output.tmp' file for further inspection",
    )
    _ = parser.add_argument(
        "--clean",
        action="store_true",
        help="delete every '*.output.tmp' file under the test root and exit without running any tests",
    )
    return parser


def main(
    argv: Sequence[str] | None = None,
    *,
    source_root: Path | None = None,
    test_root: Path | None = None,
    cwd: Path | None = None,
    environ: Mapping[str, str] | None = None,
) -> int:
    parser = create_argument_parser()
    args = parser.parse_args(argv, namespace=ParsedArguments())

    if args.timeout <= 0:
        parser.error("--timeout must be greater than zero")

    if any(not value for value in args.fflags):
        parser.error("--fflags must not be empty")

    if args.update and not args.test_ids:
        parser.error("--update requires at least one TEST")

    if args.update_all and args.test_ids:
        parser.error("--update-all does not accept TEST arguments")

    if not (args.update or args.update_all) and args.config:
        parser.error("--config is only valid with --update or --update-all")

    if args.jobs < 0:
        parser.error("--jobs cannot be negative")

    if args.clean:
        if args.test_ids:
            parser.error("--clean does not accept TEST arguments")
        if args.dump:
            parser.error("--clean cannot be combined with --dump")
        if args.update or args.update_all:
            parser.error("--clean cannot be combined with --update or --update-all")

    actual_source_root = (source_root or Path(__file__).resolve().parents[2]).resolve()
    cli_test_root = Path(args.test_root) if args.test_root else None
    actual_test_root = (test_root or cli_test_root or actual_source_root / "tests" / "golden").resolve()
    actual_cwd = (cwd or Path.cwd()).resolve()
    actual_environ = environ if environ is not None else os.environ

    if args.clean:
        # Purely a filesystem sweep: it needs neither the CLIs nor a valid corpus.
        try:
            if not actual_test_root.is_dir():
                raise GoldenError(f"golden test directory does not exist: {actual_test_root}")
            clean_errors, removed = remove_actual_outputs(actual_test_root)
        except GoldenError as exc:
            print(f"golden: error: {exc}", file=sys.stderr)
            return 2
        for error in clean_errors:
            print(f"\nFAIL: {error}", file=sys.stderr)
        print(f"golden: removed {removed} .output.tmp file(s), {len(clean_errors)} failure(s)")
        return 1 if clean_errors else 0

    configs = _selected_matrix_values(args.config, tuple(CONFIGURATIONS), "--config", parser)

    try:
        test_index = discover_test_index(actual_test_root)
        selected_entries = select_test_entries(test_index, args.test_ids, actual_cwd)
    except GoldenError as exc:
        print(f"golden: error: {exc}", file=sys.stderr)
        return 2

    update: UpdateMode | None = None
    if args.update:
        update = UpdateMode(
            "targeted",
            frozenset(_selected_matrix_values(args.update, tuple(COMMANDS), "--update", parser)),
            frozenset(entry.test_id for entry in selected_entries),
            frozenset(configs),
        )
    elif args.update_all:
        update = UpdateMode(
            "all-existing",
            frozenset(_selected_matrix_values(args.update_all, tuple(COMMANDS), "--update-all", parser)),
        )

    try:
        all_tests = load_and_validate_tests(test_index, update)
        selected_test_ids = {entry.test_id for entry in selected_entries}
        tests = [test for test in all_tests if test.test_id in selected_test_ids]
        executables = resolve_executables(
            args.luau,
            args.luau_analyze,
            actual_source_root,
            actual_cwd,
            actual_environ,
        )
    except GoldenError as exc:
        print(f"golden: error: {exc}", file=sys.stderr)
        return 2

    print(f"luau: {executables.luau}")
    print(f"luau-analyze: {executables.analyze}")

    return run_suite(
        tests,
        configs,
        executables,
        actual_test_root,
        args.timeout,
        update,
        args.jobs,
        args.dump,
        fflags=args.fflags,
    )
