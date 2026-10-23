"""Resolution of the Luau command-line executables."""

from __future__ import annotations

import os
import shutil

from collections.abc import Mapping
from pathlib import Path

from .models import Executables, GoldenError


def _is_executable(path: Path) -> bool:
    return path.is_file() and (os.name == "nt" or os.access(path, os.X_OK))


def _executable_names(name: str) -> tuple[str, str]:
    return (name, f"{name}.exe")


def _find_named(directory: Path, name: str) -> Path | None:
    for candidate_name in _executable_names(name):
        candidate = directory / candidate_name
        if _is_executable(candidate):
            return candidate.resolve()

    return None


def _resolve_override(value: str | None, label: str, cwd: Path) -> Path | None:
    if not value:
        return None

    expanded = Path(value).expanduser()
    candidate = expanded if expanded.is_absolute() else cwd / expanded

    if _is_executable(candidate):
        return candidate.resolve()

    if len(expanded.parts) == 1:
        found = shutil.which(value)
        if found and _is_executable(Path(found)):
            return Path(found).resolve()

    raise GoldenError(f"{label} override is not an executable file: {value}")


def _common_build_directories(source_root: Path, cwd: Path) -> list[Path]:
    roots: list[Path] = []
    for base in (source_root, cwd):
        roots.extend([base / "build", base / "out" / "build", base / "luau-build"])
        roots.extend(sorted(base.glob("cmake-build-*")))

    directories: list[Path] = []
    seen: set[Path] = set()
    for root in roots:
        if not root.is_dir():
            continue

        for current, child_names, _ in os.walk(root):
            current_path = Path(current)
            try:
                depth = len(current_path.relative_to(root).parts)
            except ValueError:
                continue

            if depth >= 3:
                child_names[:] = []

            resolved = current_path.resolve()
            if resolved not in seen:
                seen.add(resolved)
                directories.append(resolved)

    return directories


def resolve_executables(
    luau_override: str | None,
    analyze_override: str | None,
    source_root: Path,
    cwd: Path,
    environ: Mapping[str, str],
) -> Executables:
    luau = _resolve_override(luau_override or environ.get("LUAU"), "luau", cwd)
    analyze = _resolve_override(analyze_override or environ.get("LUAU_ANALYZE"), "luau-analyze", cwd)

    # An explicit counterpart is the strongest hint and preserves colocated tool builds.
    if luau is not None and analyze is None:
        analyze = _find_named(luau.parent, "luau-analyze")
    elif analyze is not None and luau is None:
        luau = _find_named(analyze.parent, "luau")

    searched: list[str] = []
    searched_directories: set[Path] = set()
    for directory, description in ((cwd, "working directory"), (source_root, "Luau source root")):
        resolved_directory = directory.resolve()
        if resolved_directory in searched_directories:
            continue

        searched_directories.add(resolved_directory)
        searched.append(f"{description}: {resolved_directory}")

        if luau is None and analyze is None:
            pair_luau = _find_named(directory, "luau")
            pair_analyze = _find_named(directory, "luau-analyze")
            if pair_luau and pair_analyze:
                luau, analyze = pair_luau, pair_analyze
                break
        elif luau is None:
            luau = _find_named(directory, "luau")
        elif analyze is None:
            analyze = _find_named(directory, "luau-analyze")

    if luau is None or analyze is None:
        build_dirs = _common_build_directories(source_root, cwd)

        if luau is None and analyze is None:
            pairs: list[tuple[Path, Path]] = []
            for directory in build_dirs:
                candidate_luau = _find_named(directory, "luau")
                candidate_analyze = _find_named(directory, "luau-analyze")
                if candidate_luau and candidate_analyze:
                    pair = (candidate_luau, candidate_analyze)
                    if pair not in pairs:
                        pairs.append(pair)

            if len(pairs) > 1:
                details = "\n".join(f"  luau={left}\n  luau-analyze={right}" for left, right in pairs)
                raise GoldenError(f"ambiguous Luau build discovery; pass explicit paths:\n{details}")

            if pairs:
                luau, analyze = pairs[0]
        else:
            missing_name = "luau" if luau is None else "luau-analyze"
            matches: list[Path] = []
            for directory in build_dirs:
                candidate = _find_named(directory, missing_name)
                if candidate and candidate not in matches:
                    matches.append(candidate)

            if len(matches) > 1:
                details = "\n".join(f"  {item}" for item in matches)
                raise GoldenError(f"ambiguous {missing_name} build discovery; pass an explicit path:\n{details}")

            if matches:
                if luau is None:
                    luau = matches[0]
                else:
                    analyze = matches[0]

    if luau is None:
        found = shutil.which("luau") or shutil.which("luau.exe")
        if found and _is_executable(Path(found)):
            luau = Path(found).resolve()

    if analyze is None:
        found = shutil.which("luau-analyze") or shutil.which("luau-analyze.exe")
        if found and _is_executable(Path(found)):
            analyze = Path(found).resolve()

    if luau is None or analyze is None:
        missing: list[str] = []
        if luau is None:
            missing.append("luau")
        if analyze is None:
            missing.append("luau-analyze")

        detail = "\n".join(f"  {item}" for item in searched)
        message = f"could not resolve executable(s): {', '.join(missing)}\n"
        message += f"searched colocated pairs in:\n{detail}\n"
        message += "also searched common Make/CMake build directories and PATH"

        raise GoldenError(message)

    return Executables(luau=luau, analyze=analyze)
