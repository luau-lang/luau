"""Text decoding and newline normalization helpers."""

from pathlib import Path

from .models import GoldenError


def normalize_newlines(text: str) -> str:
    return text.replace("\r\n", "\n").replace("\r", "\n")


def read_utf8(path: Path, description: str) -> str:
    try:
        return normalize_newlines(path.read_bytes().decode("utf-8"))
    except UnicodeDecodeError as exc:
        raise GoldenError(f"{description} is not valid UTF-8: {path}: {exc}") from exc
    except OSError as exc:
        raise GoldenError(f"cannot read {description} {path}: {exc}") from exc
