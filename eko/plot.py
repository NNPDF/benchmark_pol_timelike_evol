"""Compare tables against eachother."""
import argparse
import dataclasses
import pathlib

import matplotlib.pyplot as plt
import pandas as pd
from cfg import lha_data, xgrid
from matplotlib.figure import Figure

here = pathlib.Path(__file__).parent
res = here.parent / "results"
plots = here / "plots"


@dataclasses.dataclass(frozen=True)
class Elem:
    """A given calculation."""

    name: str
    data: pd.DataFrame


def load_data(name: str, tab: int, part: int) -> Elem:
    """Load and normalize data."""
    if name.lower() == "lha":
        df = lha_data(tab, part)
    else:
        p = res / f"{name}-table{tab}-part{part}.csv"
        df = pd.read_csv(p)
        df = df.drop(df.axes[1][0], axis=1)
    return Elem(name, df)


def plot(a: Elem, b: Elem) -> Figure:
    """Compare two elements."""
    fig, fig_axes = plt.subplots(2, len(a.data.axes[1]), sharex=True, figsize=(15, 5))
    for ind, dax in enumerate(a.data.axes[1]):
        ax0 = fig_axes[0][ind]
        ax0.set_title(dax)
        ax0.semilogx(xgrid, a.data[dax], label=a.name)
        ax0.semilogx(xgrid, b.data[dax], label=b.name)
        ax1 = fig_axes[1][ind]
        ax1.semilogx(xgrid, a.data[dax] / a.data[dax])
        ax1.semilogx(xgrid, b.data[dax] / a.data[dax], label=f"{b.name}/{a.name}")
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
    args = parser.parse_args()

    a = load_data(args.a, args.table, args.part)
    b = load_data(args.b, args.table, args.part)
    fig = plot(a, b)
    fig.savefig(plots / f"table{args.table}-part{args.part}-{args.a}-{args.b}.pdf")
    plt.close(fig)


if __name__ == "__main__":
    main()
