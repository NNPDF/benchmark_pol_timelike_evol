"""Collection of helper functions."""
import dataclasses
import pathlib

import pandas as pd
import yaml
from ekomark.benchmark.external.LHA_utils import here as there

here = pathlib.Path(__file__).parent
res = here.parent / "results"

# reference values
with open(there / "LHA_polarized.yaml", encoding="utf-8") as o:
    ref_data = yaml.safe_load(o)


def lha_data(tab: str, part: str) -> pd.DataFrame:
    """Load LHA data from ekomark."""
    df = pd.DataFrame(ref_data[f"table{tab}"][f"part{part}"])
    df["d_v"] *= -1.0
    df["L_m"] *= -2.0
    df["L_p"] *= -1.0
    return df


@dataclasses.dataclass(frozen=True)
class Elem:
    """A given calculation."""

    name: str
    data: pd.DataFrame


def load_data(name: str, tab: int, part: int, tl: bool = False) -> Elem:
    """Load data from disk or ekomark."""
    if name.lower() == "lha":
        df = lha_data(tab, part)
    else:
        suffix = "-tl" if tl else ""
        p = res / f"{name}-table{tab}-part{part}{suffix}.csv"
        df = pd.read_csv(p)
        df = df.drop(df.axes[1][0], axis=1)
    return Elem(name, df)
