"""Concurrent orchestration of golden-test matrix runs."""

from __future__ import annotations

import sys

from collections.abc import Sequence
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from .execution import execute_configuration
from .expectations import update_configuration, validate_configuration, write_actual_outputs
from .models import ConfigResult, Executables, GoldenTest, UpdateMode


def run_suite(
    tests: Sequence[GoldenTest],
    configs: Sequence[str],
    executables: Executables,
    test_root: Path,
    timeout: float,
    update: UpdateMode | None,
    jobs: int,
    dump: bool = False,
    fflags: Sequence[str] = (),
) -> int:
    # Each `(test, config)` run is an independent batch of processes for the CLI, so they
    # execute will concurrently. Validation and file writes then run sequentially in
    # the original sorted order, keeping output and update behavior deterministic
    # regardless of jobs.
    #
    # Negative values for `jobs` will auto-size the pool, and a value of `1` uses serial execution.
    work = [(test, config) for test in tests for config in configs]

    def execute(item: tuple[GoldenTest, str]) -> ConfigResult:
        return execute_configuration(item[0], item[1], executables, test_root, timeout, fflags)

    if jobs == 1 or len(work) <= 1:
        results = [execute(item) for item in work]
    else:
        with ThreadPoolExecutor(max_workers=jobs if jobs > 0 else None) as pool:
            results = list(pool.map(execute, work))

    errors: list[str] = []
    writes = 0
    actual_writes = 0
    for (test, config), result in zip(work, results, strict=True):
        errors.extend(validate_configuration(test, config, result, update))
        if update is not None:
            update_errors, update_writes = update_configuration(test, config, result, update)
            errors.extend(update_errors)
            writes += update_writes
        if dump:
            actual_errors, written = write_actual_outputs(test, config, result)
            errors.extend(actual_errors)
            actual_writes += written

    for error in errors:
        print(f"\nFAIL: {error}", file=sys.stderr)
    update_summary = f", updated {writes} file(s)" if update is not None else ""
    actual_summary = f", dumped {actual_writes} .output.tmp file(s)" if dump else ""
    print(
        f"golden: {len(tests)} test(s), {len(work)} configuration run(s)"
        f"{update_summary}{actual_summary}, {len(errors)} failure(s)"
    )
    return 1 if errors else 0
