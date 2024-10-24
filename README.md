# wrf_num_procs.py

**A script to find the minimum and maximum number of processes for running WRF and plot schematic of domain decomposition.**

## Overview

`wrf_num_procs.py` determines the minimum and maximum number of processes and nodes based on the domain's grid points in the i/j directions. 
It ensures that each processor handles at least a minimum number of grid points (i.e. 10 grids/tasks), optimizing computational resources for WRF simulations. 

This script is adapted from a discussion on the [UCAR MMM Forum](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/).

## Usage
This script can read in the domain size from a namelist file or from the command line.

Example:

```
    ./wrf_num_procs.py --e_we 320 --e_sn 180
```
Example Output:
```
========================================
Domain 1: e_we=320, e_sn=180
Minimum number of processors: 3 --> Requiring 1 nodes using (3 procs/node)
Maximum number of processors: 256 --> Requiring 2 nodes using (128 procs/node)
----------------------------------------
Domain Decomposition Layout with 256 Processes:
False
Summary:
  Base tile size : 20x11 (each tile has 20 x 11 grid points)
  Total tiles    : 16 x 16
  Extra row with tile size    : 20 x 4 (added to the bottom)
----------------------------------------
```


#### Using Namelist
The script can read the domain size from a namelist file and find the minimum and maximum number of processes for each domain.

```
    ./wrf_num_procs.py --namelist namelist.input

```

## Domain Decomposition Schematic
The script can also plot a schematic of the domain decomposition using the `--decomp` flag or `--decomp_schematic` flag.

The goal is to help with decomposition imbalance and visualize how the domain is decomposed into tiles and how many grid points are assigned to each processor.

Example:
```
./wrf_num_procs.py --e_we 320 --e_sn 180 --cores 128 --decomp
========================================
Domain 1: e_we=320, e_sn=180
Minimum number of processors: 3 --> Requiring 1 nodes using (3 procs/node)
Maximum number of processors: 256 --> Requiring 2 nodes using (128 procs/node)
----------------------------------------
Domain Decomposition Layout with 256 Processes:
True
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11 | 20x11
20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4 | 20x4
----------------------------------------
Summary:
  Base tile size : 20x11 (each tile has 20 x 11 grid points)
  Total tiles    : 16 x 16
  Extra row with tile size    : 20 x 4 (added to the bottom)
----------------------------------------
```


## Arguments
The script requires the following arguments:
```
./wrf_num_procs.py --help
usage: wrf_num_procs.py [-h] [--cores CORES] [--e_we E_WE [E_WE ...]] [--e_sn E_SN [E_SN ...]] [--namelist NAMELIST] [--decomp] [--debug]

Determine the minimum and maximum number of processors and nodes based on number grid points.

options:
  -h, --help            show this help message and exit
  --cores CORES         Number of cores per node (e.g., Derecho: 128 cores/node) [default = 128]
  --e_we E_WE [E_WE ...]
                        e_we values if not using namelist
  --e_sn E_SN [E_SN ...]
                        e_sn values if not using namelist
  --namelist NAMELIST   Path to namelist file containing e_we and e_sn
  --decomp, --decomp_schematic
                        Print domain decomposition schematic!
  --debug               Enable debug mode for detailed output

```