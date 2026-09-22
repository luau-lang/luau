"""Module entry point for ``python -m tools.golden``."""

import sys

from .cli import main

if __name__ == "__main__":
    sys.exit(main())
