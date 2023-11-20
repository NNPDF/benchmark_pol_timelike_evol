"""Compare tables against eachother."""
import argparse
import dataclasses
import pathlib

import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.figure import Figure

from cfg import xgrid

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
    p = res / f"{name}-table{tab}-part{part}.csv"
    df = pd.read_csv(p)
    # fix d_v, L_m, and L_p to be consistent with unpolarized
    if name != "eko":
        df["d_v"] *= -1.0
        df["L_m"] *= -0.5
        df["L_p"] *= -1.0
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("table", help="LH table")
    parser.add_argument("part", help="table part")
    args = parser.parse_args()

    a = load_data("eko", args.table, args.part)
    b = load_data("apfelxx", args.table, args.part)
    fig = plot(a, b)
    fig.savefig(plots / f"table{args.table}-part{args.part}.pdf")
    plt.close(fig)
