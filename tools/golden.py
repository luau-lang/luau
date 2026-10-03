"""Buck bootstrap entry point for the Luau golden test runner."""

import sys

from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from Client.Luau.tools.golden.cli import main
else:
    from golden.cli import main


if __name__ == "__main__":
    sys.exit(main())
