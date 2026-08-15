"""Entry point for the ModSIM GUI.

Run with::

    python -m modsim
"""

import sys


def main() -> int:
    from modsim.gui.app import run

    return run(sys.argv[1:])


if __name__ == "__main__":
    sys.exit(main())
