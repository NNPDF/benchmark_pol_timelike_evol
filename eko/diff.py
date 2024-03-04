"""Compare tables against eachother."""
import argparse

from utils import load_data


def main() -> None:
    """CLI entry point"""
    parser = argparse.ArgumentParser()
    parser.add_argument("table", help="LH table")
    parser.add_argument("part", help="table part")
    parser.add_argument("a", help="first operand")
    parser.add_argument("b", help="second operand")
    parser.add_argument("--print-a", help="Print a", action="store_true")
    parser.add_argument("--print-b", help="Print b", action="store_true")
    parser.add_argument(
        "--no-rel", help="Suppress relative difference", action="store_true"
    )
    parser.add_argument(
        "--no-abs", help="Suppress absolute difference", action="store_true"
    )
    parser.add_argument(
        "--print-combined", help="Print combined check", action="store_true"
    )
    parser.add_argument(
        "--combined-rel", help="Combined check rel. epsilon", default=1e-3, type=float
    )
    parser.add_argument(
        "--combined-abs", help="Combined check abs. epsilon", default=1e-5, type=float
    )
    args = parser.parse_args()

    # load
    a = load_data(args.a, args.table, args.part)
    b = load_data(args.b, args.table, args.part)
    # print absolute values
    for opt, el in ((args.print_a, a), (args.print_b, b)):
        if opt:
            print(el.name)
            print(el.data)
            print()
    # rel. diff
    if not args.no_rel:
        print(f"rel. difference {a.name} to {b.name}")
        print((a.data - b.data) / b.data)
        print()
    # abs. diff
    if not args.no_abs:
        print(f"abs. difference {a.name} to {b.name}")
        print(a.data - b.data)
        print()
    # combined table
    if args.print_combined:
        print(
            f"combined difference {a.name} to {b.name} with rel. < {args.combined_rel} and abs. < {args.combined_abs}"
        )
        rel_e = ((a.data - b.data) / b.data) < args.combined_rel
        abs_e = (a.data - b.data) < args.combined_abs
        print(rel_e | abs_e)


if __name__ == "__main__":
    main()
