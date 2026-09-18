"""Buck bootstrap entry point for the Luau golden harness self-tests."""

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from Client.Luau.tools.golden.selftest import run_selftests
else:
    from golden.selftest import run_selftests


if __name__ == "__main__":
    run_selftests()
