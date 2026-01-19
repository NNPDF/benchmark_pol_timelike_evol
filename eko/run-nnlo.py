"""Generate NNLO tables."""

import argparse
import logging
import pathlib
import sys

import pandas as pd
from banana import toy
from cfg import (
    SQRT2,
    ffns_operator,
    ffns_theory,
    vfns_labels,
    vfns_operator,
    vfns_rotate_to_LHA,
    vfns_theory,
    xgrid,
)
from ekobox import apply

import eko
from eko.runner.managed import solve


def main():
    """CLI entry point."""
    parser = argparse.ArgumentParser()
    parser.add_argument("scheme", help="FFNS or aVFNS or VFNS?")
    parser.add_argument("sv", help="scale variation: up, central, or down")
    parser.add_argument(
        "-tl", "--timelike", action="store_true", help="Time-like evolution?"
    )
    parser.add_argument("--rerun", help="Rerun eko", action="store_true")
    parser.add_argument(
        "-v", "--verbose", help="Print eko log to screen", action="store_true"
    )
    args = parser.parse_args()
    suffix = "-tl" if args.timelike else ""

    # determine xif
    if "central".startswith(args.sv):
        xif = 1.0
        sv = "central"
        part = 1
    elif "up".startswith(args.sv):
        xif = SQRT2
        sv = "up"
        part = 2
    elif "down".startswith(args.sv):
        xif = 1.0 / SQRT2
        sv = "down"
        part = 3
    else:
        raise ValueError(
            "sv has to be up, central, or down - or any abbreviation there of"
        )

    # determine scheme
    matching_qcd = 2
    if args.scheme == "FFNS":
        t = ffns_theory(xif)
        o = ffns_operator(args.timelike)
        tab = 19
    elif args.scheme == "aVFNS":
        t = vfns_theory(xif)
        o = vfns_operator(args.timelike)
        tab = 20
        matching_qcd = 1
    elif args.scheme == "VFNS":
        t = vfns_theory(xif)
        o = vfns_operator(args.timelike)
        tab = 21
    else:
        raise ValueError("scheme has to be FFNS or aVFNS or VFNS")
    t.order = (3, 0)
    t.matching_order = (matching_qcd, 0)
    lab = vfns_labels
    rot = vfns_rotate_to_LHA

    # eko path
    p = pathlib.Path(f"NNLO-{args.scheme}-{sv}{suffix}.tar")

    # recompute?
    if not p.exists() or args.rerun:
        print("(Re)running eko ...")
        p.unlink(True)
        if args.verbose:
            log_stdout = logging.StreamHandler(sys.stdout)
            log_stdout.setLevel(logging.INFO)
            log_stdout.setFormatter(logging.Formatter("%(message)s"))
            logging.getLogger("eko").handlers = []
            logging.getLogger("eko").addHandler(log_stdout)
            logging.getLogger("eko").setLevel(logging.INFO)
        solve(t, o, p)

    # apply PDF
    out = {}
    xgrid_ = xgrid(args.timelike)
    with eko.EKO.read(p) as eko_:
        pdf = apply.apply_pdf_flavor(
            eko_,
            toy.mkPDF("ToyFF_unpolarized" if args.timelike else "ToyLH_polarized", 0),
            lab,
            xgrid_,
            rot,
        )
        for lab, f in list(pdf[0].values())[0].items():
            out[lab] = xgrid_ * f

    # display result
    pd.set_option("display.float_format", "{:.4e}".format)
    me = pd.DataFrame(out)
    print("EKO")
    print(me)
    # dump to file
    me.to_csv(f"../results/eko-table{tab}-part{part}{suffix}.csv")


if __name__ == "__main__":
    main()
