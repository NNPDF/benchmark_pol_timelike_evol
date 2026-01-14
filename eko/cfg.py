"""Runcard configurations."""

import copy
from math import inf, nan

import numpy as np
import numpy.typing as npt

from eko import basis_rotation as br
from eko.interpolation import lambertgrid
from eko.io import runcards
from eko.io.types import ReferenceRunning

SQRT2 = np.sqrt(2.0)

# theory settings
# ---------------
_t_vfns = dict(
    order=[1, 0],
    couplings=dict(alphas=0.35, alphaem=0.007496, ref=[SQRT2, 3]),
    heavy=dict(
        masses=[ReferenceRunning([mq, nan]) for mq in (SQRT2, 4.5, 175.0)],
        masses_scheme="POLE",
        matching_ratios=[1.0, 1.0, 1.0],
    ),
    xif=1.0,
    n3lo_ad_variation=(0, 0, 0, 0, 0, 0, 0),
    matching_order=[0, 0],
)


def vfns_theory(xif=1.0):
    """Generate a VFNS theory card."""
    tt = copy.deepcopy(_t_vfns)
    tt["xif"] = xif
    return runcards.TheoryCard.from_dict(tt)


_t_ffns = copy.deepcopy(_t_vfns)
_t_ffns["couplings"]["ref"][1] = 4
_t_ffns["heavy"]["masses"] = [
    ReferenceRunning([0, nan]),
    ReferenceRunning([inf, nan]),
    ReferenceRunning([inf, nan]),
]


def ffns_theory(xif=1.0):
    """Generate a VFNS theory card."""
    tt = copy.deepcopy(_t_ffns)
    tt["xif"] = xif
    return runcards.TheoryCard.from_dict(tt)


# operator settings
# -----------------
_o_vfns = dict(
    init=[SQRT2, 3],
    mugrid=[(100.0, 5)],
    xgrid=lambertgrid(60).tolist(),
    configs=dict(
        evolution_method="iterate-exact",
        ev_op_max_order=[10, 0],
        ev_op_iterations=30,
        interpolation_polynomial_degree=4,
        interpolation_is_log=True,
        scvar_method="exponentiated",
        inversion_method=None,
        n_integration_cores=-2,
        polarized=True,
        time_like=False,
    ),
    debug=dict(
        skip_singlet=False,
        skip_non_singlet=False,
    ),
)


def vfns_operator(tl: bool = False) -> runcards.OperatorCard:
    """Generate a VFNS operator card."""
    oo = copy.deepcopy(_o_vfns)
    if tl:
        oo["configs"]["polarized"] = False
        oo["configs"]["time_like"] = True
    return runcards.OperatorCard.from_dict(oo)


_o_ffns = copy.deepcopy(_o_vfns)
_o_ffns["mugrid"] = [(100.0, 4)]
_o_ffns["init"][1] = 4


def ffns_operator(tl: bool = False) -> runcards.OperatorCard:
    """Generate a FFNS operator card."""
    oo = copy.deepcopy(_o_ffns)
    if tl:
        oo["configs"]["polarized"] = False
        oo["configs"]["time_like"] = True
    return runcards.OperatorCard.from_dict(oo)


# flavor rotations
# ----------------
# Let's use the normalization of Table 16+18 - note that Table 17
# is again different ... sigh

vfns_labels = ["u_v", "d_v", "L_m", "L_p", "s_p", "c_p", "b_p", "g"]
vfns_rotate_to_LHA = np.zeros((len(vfns_labels), 14))
# u_v = u - ubar
vfns_rotate_to_LHA[0][br.flavor_basis_pids.index(-2)] = -1
vfns_rotate_to_LHA[0][br.flavor_basis_pids.index(2)] = 1
# d_v = d - dbar
vfns_rotate_to_LHA[1][br.flavor_basis_pids.index(-1)] = -1 * -1
vfns_rotate_to_LHA[1][br.flavor_basis_pids.index(1)] = 1 * -1
# L_- = dbar - ubar
vfns_rotate_to_LHA[2][br.flavor_basis_pids.index(-1)] = 1 * -2
vfns_rotate_to_LHA[2][br.flavor_basis_pids.index(-2)] = -1 * -2
# 2L_+ = 2dbar + 2ubar
vfns_rotate_to_LHA[3][br.flavor_basis_pids.index(-1)] = 2 * -1
vfns_rotate_to_LHA[3][br.flavor_basis_pids.index(-2)] = 2 * -1
# s_+ = s + sbar
vfns_rotate_to_LHA[4][br.flavor_basis_pids.index(-3)] = 1
vfns_rotate_to_LHA[4][br.flavor_basis_pids.index(3)] = 1
# c_+ = c + cbar
vfns_rotate_to_LHA[5][br.flavor_basis_pids.index(-4)] = 1
vfns_rotate_to_LHA[5][br.flavor_basis_pids.index(4)] = 1
# b_+ = b + bbar
vfns_rotate_to_LHA[6][br.flavor_basis_pids.index(-5)] = 1
vfns_rotate_to_LHA[6][br.flavor_basis_pids.index(5)] = 1
# g = g
vfns_rotate_to_LHA[7][br.flavor_basis_pids.index(21)] = 1


# setup x rotation
def xgrid(tl: bool = False) -> npt.NDArray:
    """Generate target grid."""
    if tl:
        return np.array(
            [1e-2, 5e-2, 1e-1, 2e-1, 3e-1, 4e-1, 5e-1, 6e-1, 7e-1, 8e-1, 9e-1]
        )
    return np.array([1e-7, 1e-6, 1e-5, 1e-4, 1e-3, 1e-2, 0.1, 0.3, 0.5, 0.7, 0.9])
