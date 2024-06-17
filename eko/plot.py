"""Compare tables against eachother."""
import argparse
from collections.abc import Collection

import matplotlib.pyplot as plt
from cfg import xgrid
from matplotlib.figure import Figure
from utils import Elem, here, load_data

plots = here / "plots"


def plot(xgrid_: Collection[float], a: Elem, b: Elem) -> Figure:
    """Compare two elements."""
    fig, fig_axes = plt.subplots(2, len(a.data.axes[1]), sharex=True, figsize=(15, 5))
    for ind, dax in enumerate(a.data.axes[1]):
        ax0 = fig_axes[0][ind]
        ax0.set_title(dax)
        ax0.semilogx(xgrid_, a.data[dax], label=a.name)
        ax0.semilogx(xgrid_, b.data[dax], label=b.name)
        ax1 = fig_axes[1][ind]
        ax1.semilogx(xgrid_, a.data[dax] / a.data[dax])
        ax1.semilogx(xgrid_, b.data[dax] / a.data[dax], label=f"{b.name}/{a.name}")
    fig_axes[0][0].legend()
    fig_axes[1][0].legend()
    return fig


def main() -> None:
    """CLI entry point"""
    parser = argparse.ArgumentParser()
    parser.add_argument("table", help="LH table")
    parser.add_argument("part", help="table part")
    parser.add_argument("a", help="first operand")
    parser.add_argument("b", help="second operand")
    parser.add_argument(
        "-tl", "--timelike", action="store_true", help="Time-like evolution?"
    )
    args = parser.parse_args()

    a = load_data(args.a, args.table, args.part, args.timelike)
    b = load_data(args.b, args.table, args.part, args.timelike)
    fig = plot(xgrid(args.timelike), a, b)
    suffix = "-tl" if args.timelike else ""
    fig.savefig(
        plots / f"table{args.table}-part{args.part}-{args.a}-{args.b}{suffix}.pdf"
    )
    plt.close(fig)


if __name__ == "__main__":
    main()
