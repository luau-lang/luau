"""Text decoding, newline normalization, and diff formatting helpers."""

import difflib

from collections.abc import Mapping
from pathlib import Path

from .models import GoldenError

from typing import TextIO

try:
    import colorama
except ImportError:
    colorama = None

MAX_HIGHLIGHT_LINE_LENGTH = 4096

# The 256-color cube avoids theme-defined ANSI normal/bright colors, which can
# have identical shades. Each highlight is darker than its line background.
DIFF_FOREGROUND = "\x1b[38;5;16m"
REMOVED_BACKGROUND = "\x1b[48;5;224m"
REMOVED_HIGHLIGHT = "\x1b[48;5;217m"
ADDED_BACKGROUND = "\x1b[48;5;194m"
ADDED_HIGHLIGHT = "\x1b[48;5;157m"


def diff_color_enabled(stream: TextIO, environ: Mapping[str, str]) -> bool:
    """Enable optional terminal colors, honoring even an empty NO_COLOR value."""
    if colorama is None or "NO_COLOR" in environ or environ.get("TERM") == "dumb" or not stream.isatty():
        return False
    colorama.just_fix_windows_console()
    return True


def _highlight_pair(removed: str, added: str) -> tuple[str, str]:
    if colorama is None:
        return removed, added

    # SequenceMatcher can be quadratic; long diagnostics still get line colors.
    if max(len(removed), len(added)) > MAX_HIGHLIGHT_LINE_LENGTH:
        return removed, added

    matcher = difflib.SequenceMatcher(None, removed, added, autojunk=False)
    old_parts: list[str] = []
    new_parts: list[str] = []
    for tag, old_start, old_end, new_start, new_end in matcher.get_opcodes():
        old = removed[old_start:old_end]
        new = added[new_start:new_end]
        if tag != "equal":
            if old:
                old = f"{REMOVED_HIGHLIGHT}{old}{REMOVED_BACKGROUND}"
            if new:
                new = f"{ADDED_HIGHLIGHT}{new}{ADDED_BACKGROUND}"
        old_parts.append(old)
        new_parts.append(new)
    return "".join(old_parts), "".join(new_parts)


def unified_diff(expected_path: Path, expected: str, actual: str, *, color: bool = False) -> str:
    """Format a unified diff; optional color affects only the displayed text."""
    lines = list(
        difflib.unified_diff(
            expected.splitlines(keepends=True),
            actual.splitlines(keepends=True),
            fromfile=str(expected_path),
            tofile="captured output",
        )
    )
    if not lines:
        return f"expected {expected!r}, captured {actual!r}"

    if color and colorama is not None:
        # Pair replacements within each change block, excluding the file headers.
        index = 2
        while index < len(lines):
            if not lines[index].startswith("-"):
                index += 1
                continue
            old_start = index
            while index < len(lines) and lines[index].startswith("-"):
                index += 1
            new_start = index
            while index < len(lines) and lines[index].startswith("+"):
                index += 1
            for old_index, new_index in zip(range(old_start, new_start), range(new_start, index), strict=False):
                old, new = _highlight_pair(
                    lines[old_index][1:].removesuffix("\n"), lines[new_index][1:].removesuffix("\n")
                )
                lines[old_index] = "-" + old + ("\n" if lines[old_index].endswith("\n") else "")
                lines[new_index] = "+" + new + ("\n" if lines[new_index].endswith("\n") else "")

        for index, line in enumerate(lines):
            if line.startswith("-"):
                style = colorama.Fore.RED if index < 2 else DIFF_FOREGROUND + REMOVED_BACKGROUND
            elif line.startswith("+"):
                style = colorama.Fore.GREEN if index < 2 else DIFF_FOREGROUND + ADDED_BACKGROUND
            else:
                continue
            ending = "\n" if line.endswith("\n") else ""
            lines[index] = style + line.removesuffix("\n") + colorama.Style.RESET_ALL + ending

    return "".join(lines)


def normalize_newlines(text: str) -> str:
    return text.replace("\r\n", "\n").replace("\r", "\n")


def read_utf8(path: Path, description: str) -> str:
    try:
        return normalize_newlines(path.read_bytes().decode("utf-8"))
    except UnicodeDecodeError as exc:
        raise GoldenError(f"{description} is not valid UTF-8: {path}: {exc}") from exc
    except OSError as exc:
        raise GoldenError(f"cannot read {description} {path}: {exc}") from exc
