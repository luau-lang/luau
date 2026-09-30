# Golden harness meta tests

The tests in this directory exercise the golden test framework itself rather
than serving as regression tests for Luau language behavior.

They cover configuration-specific status resolution; strict, nonstrict, and
runtime failures; global and scoped regex assertions; exact-output files; empty
outputs; and multifile test discovery. New language behavior tests should
normally live elsewhere under `tests/golden/`.
