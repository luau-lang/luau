"""Concurrent orchestration of golden-test matrix runs."""

from __future__ import annotations

import os
import sys

from collections.abc import Mapping, Sequence
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from .execution import execute_configuration
from .expectations import update_configuration, validate_configuration, write_actual_outputs, write_snapshot
from .models import ConfigResult, Executables, GoldenTest, UpdateMode
from .text import diff_color_enabled


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
    environ: Mapping[str, str] | None = None,
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

    results_by_test: dict[str, dict[str, ConfigResult]] = {}
    for (test, config), result in zip(work, results, strict=True):
        results_by_test.setdefault(test.test_id, {})[config] = result

    errors: list[str] = []
    color = diff_color_enabled(sys.stderr, os.environ if environ is None else environ)
    updated_outputs = 0
    updated_files = 0
    dumped_files = 0
    for test in tests:
        test_results = results_by_test[test.test_id]
        test_updates = 0
        for config, result in test_results.items():
            errors.extend(validate_configuration(test, config, result, update, color=color))
            if update is not None:
                test_updates += update_configuration(test, config, result, update)

        # Every configuration lands in the same snapshot, so it is written once per test.
        if test_updates:
            if (error := write_snapshot(test)) is not None:
                errors.append(error)
            else:
                updated_outputs += test_updates
                updated_files += 1
        if dump:
            if (error := write_actual_outputs(test, test_results)) is not None:
                errors.append(error)
            else:
                dumped_files += 1

    for error in errors:
        print(f"\nFAIL: {error}", file=sys.stderr)
    update_summary = f", updated {updated_outputs} output(s) in {updated_files} file(s)" if update is not None else ""
    actual_summary = f", dumped {dumped_files} .golden.toml.tmp file(s)" if dump else ""
    print(
        f"golden: {len(tests)} test(s), {len(work)} configuration run(s)"
        f"{update_summary}{actual_summary}, {len(errors)} failure(s)"
    )
    return 1 if errors else 0
