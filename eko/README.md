# EKO implementation

## Installation
Create a new virtual environment and just run `pip install -r requirements.txt`.

## Running
Execute `python run-lo.py` (or `python run-nlo.py`) and check the help message.

Be aware that for the first execution in the environment eko needs to compile the
numba functions which can take some time (~15min). It might be wise to adjust the
`n_integration_cores` parameter  of the operator settings in `cfg.py` to `1`.
This is also true for the first time matching conditions are invoked.

## Plotting
Execute `python plot.py` and check the help message.
