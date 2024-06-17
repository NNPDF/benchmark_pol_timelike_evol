"""Benchmark LO tables from LHA."""
import argparse
import logging
import pathlib
import sys

import pandas as pd
from banana import toy
from cfg import (
    ffns_operator,
    ffns_theory,
    vfns_labels,
    vfns_operator,
    vfns_rotate_to_LHA,
    vfns_theory,
    xgrid,
)
from ekobox import apply
from utils import lha_data

import eko
from eko.runner.managed import solve


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("scheme", help="FFNS or VFNS?")
    parser.add_argument(
        "-tl", "--timelike", action="store_true", help="Time-like evolution?"
    )
    parser.add_argument("--rerun", help="Rerun eko", action="store_true")
    parser.add_argument(
        "-v", "--verbose", help="Print eko log to screen", action="store_true"
    )
    args = parser.parse_args()
    suffix = "-tl" if args.timelike else ""

    tab = 16
    # determine scheme
    if args.scheme == "FFNS":
        scheme = "FFNS"
        t = ffns_theory(1.0)
        o = ffns_operator(args.timelike)
        part = 2
    elif args.scheme == "VFNS":
        scheme = "VFNS"
        t = vfns_theory(1.0)
        o = vfns_operator(args.timelike)
        part = 3
    else:
        raise ValueError("scheme has to be FFNS or VFNS")
    lab = vfns_labels
    rot = vfns_rotate_to_LHA

    # eko path
    p = pathlib.Path(f"LO-{scheme}{suffix}.tar")

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
            xgrid_,
            rot,
            lab,
        )
        for lab, f in list(pdf.values())[0]["pdfs"].items():
            out[lab] = xgrid_ * f

    # display result
    pd.set_option("display.float_format", "{:.4e}".format)
    me = pd.DataFrame(out)
    print("EKO")
    print(me)
    # dump to file
    me.to_csv(f"../results/eko-table{tab}-part{part}{suffix}.csv")

    if not args.timelike:
        # load reference
        ref = lha_data(tab, part)
        print()
        print("rel. distance to reference")
        print((me - ref) / ref)


if __name__ == "__main__":
    main()
