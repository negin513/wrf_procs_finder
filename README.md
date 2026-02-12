# WRF Processor/MPI Rank Decomposition Tool

[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

A tool to determine the valid procs range for WRF simulations and visualize domain decomposition.

This matches WRF's actual validation logic from `share/module_check_a_mundo.F`.



## Overview

`wrf-procs-finder` analyzes WRF domain configurations to determine:
- Minimum and maximum number of procs based on grid dimensions
- Decomposition layout for parallel execution
- Node requirements based on procs per node

The tool ensures each processor handles at least 10 grid points (the WRF minimum for adequate computation space beyond halo regions).

This script is adapted from a discussion on the [UCAR MMM Forum](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/).

## Installation

```bash
git clone https://github.com/negin513/wrf_procs_finder.git
cd wrf_procs_finder
```


## Quick Start

```bash
# From a namelist file
wrf-procs-finder --namelist examples/namelist.input

# From grid dimensions directly
wrf-procs-finder --e_we 320 --e_sn 180

# With visual decomposition schematic
wrf-procs-finder --namelist examples/namelist.input --decomp
```

## Background

### WRF Domain Decomposition

When running WRF in parallel, the domain is divided into tiles (one per processor):

```
┌─────────────────────────────────────────┐
│  Domain (e_we × e_sn grid points)       │
│  ┌─────┬─────┬─────┬─────┐              │
│  │tile │tile │tile │tile │  ← nproc_y   │
│  ├─────┼─────┼─────┼─────┤    tiles     │
│  │tile │tile │tile │tile │              │
│  └─────┴─────┴─────┴─────┘              │
│       ↑ nproc_x tiles                   │
└─────────────────────────────────────────┘
```

Each tile has:
- **Halo regions**: 5 rows/columns on each side for inter-processor communication
- **Computation space**: The interior region where actual calculations occur

Here is a tile structure:

```
              Single Tile Structure
  ┌─────────────────────────────────────────┐
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│ ─┐
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │ 5 rows
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │ (halo)
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│ ─┘
  │░░░░░┌─────────────────────────────┐░░░░░│
  │░░░░░│                             │░░░░░│
  │░░░░░│                             │░░░░░│
  │░░░░░│     COMPUTATION SPACE       │░░░░░│
  │░░░░░│     (interior region)       │░░░░░│
  │░░░░░│                             │░░░░░│
  │░░░░░│                             │░░░░░│
  │░░░░░└─────────────────────────────┘░░░░░│
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│ ─┐
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │ 5 rows
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │ (halo)
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│  │
  │░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░│ ─┘
  └─────────────────────────────────────────┘
   ├───┤                             ├───┤
   5 cols                            5 cols
   (halo)                            (halo)

  ░ = Halo region (inter-processor communication)
```

WRF requires each tile to have ≥ 10 grid points in both directions. This ensures adequate computation space beyond the halo regions.

From WRF source (`share/module_check_a_mundo.F`):

```fortran
IF ( ( e_we / nproc_x .LT. 10 ) .OR. ( e_sn / nproc_y .LT. 10 ) ) THEN
    ! FATAL ERROR
```


If you have fewer than 10 grid points in either direction, WRF will fail with an error like this: 
```
For domain 1 , the domain size is too small for this many processors, or
the decomposition aspect ratio is poor.
Minimum decomposed computational patch size, either x-dir or y-dir, is 10 grid cells.
e_we = 75, nproc_x = 10, with cell width in x-direction = 7
e_sn = 70, nproc_y = 10, with cell width in y-direction = 7
--- ERROR: Reduce the MPI rank count, or redistribute the tasks.
-------------- FATAL CALLED ---------------
FATAL CALLED FROM FILE:  <stdin>  LINE:    XXXX
NOTE: 1 namelist settings are wrong. Please check and reset these options
-------------------------------------------
```

### Domain Decomposition Strategy

WRF decomposes the domain by finding the two closest factors of the processor count to create near-square tile layouts. For example, 16 processors become 4×4 in x and y directions (not 2×8), since 4 and 4 are the closest factor pair.


Square layouts minimize inter-processor communication because they have the smallest perimeter-to-area ratio, but users can override this by setting `nproc_x` and `nproc_y` explicitly in `namelist.input`. 

- 16 processors → 4×4 tiles (good) 
- 16 processors → 8×2 tiles (bad)
- 11 processors → 1×11 tiles (poor - prime number)

Avoid prime numbers when choosing processor counts —> they always result in 1×N layouts.

## Example Output

Running with the example namelist (`wrf-procs-finder --namelist examples/namelist.input`):

```
------------------------------------------------------------
  DOMAIN 1 of 2
------------------------------------------------------------
  e_we = 150
  e_sn = 130
  Total grid points: 150 x 130 = 19,500

  nprocs range:
    Minimum.......................      1 procs  =>  1 node @ 1 procs/node
    Maximum.......................    128 procs  =>  1 node @ 128 procs/node

  Decomposition for 128 procs:
      Layout    :    8 x 16  tiles
      Avg tile  :  ~18 x 8   grid points

------------------------------------------------------------
  DOMAIN 2 of 2
------------------------------------------------------------
  e_we = 220
  e_sn = 214
  Total grid points: 220 x 214 = 47,080

  nprocs range:
    Minimum.......................      4 procs  =>  1 node @ 4 procs/node
    Maximum.......................    384 procs  =>  3 nodes @ 128 procs/node

  Decomposition for 384 procs:
      Layout    :   16 x 24  tiles
      Avg tile  :  ~13 x 8   grid points

============================================================
  COMBINED NPROCS RANGE (all domains)
============================================================
  nprocs range:
    Minimum (largest domain)......      4 procs  =>  1 node @ 4 procs/node
    Maximum (smallest domain).....    128 procs  =>  1 node @ 128 procs/node
```

### With Decomposition Schematic (--decomp)

```
wrf-procs-finder --e_we 180 --e_sn 200 --decomp



------------------------------------------------------------
  DOMAIN 1 of 1
------------------------------------------------------------
  e_we = 180
  e_sn = 200
  Total grid points: 180 x 200 = 36,000

  nprocs range:
    Minimum.......................      2 procs  =>  1 node @ 2 procs/node
    Maximum.......................    256 procs  =>  2 nodes @ 128 procs/node

  Decomposition for 256 procs:

    ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │ 11x12 │  4x12 │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  11x8 │  4x8  │
    └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘

      Layout    :   16 x 16  tiles
      Avg tile  : ~ 11 x 12  grid points

============================================================
  NOTES
============================================================
  ⚠ The maximum nprocs shown is the UPPER LIMIT that avoids
    decomposition errors (each tile ≥ 10 grid points).

    This does NOT mean it's the optimal choice!

    Start with a value in the middle of the valid range,
    then benchmark to find the best performance.
============================================================
```


## Command Line Arguments

| Argument | Description |
|----------|-------------|
| `--cores` | Number of cores per node |
| `--e_we` | Grid points in west-east direction (one or more values) |
| `--e_sn` | Grid points in south-north direction (one or more values) |
| `--namelist` | Path to WRF namelist file |
| `--decomp` | Show domain decomposition schematic |
| `--ascii` | Use ASCII characters for schematic borders |
| `--verbose`, `-v` | Show detailed output (remainders, namelist settings) |
| `--quiet`, `-q` | Suppress notes and warnings |

## Derecho Support

When running on NCAR's Derecho supercomputer, the tool automatically generates PBS select lines. For example: 

```
  Derecho PBS (for max procs):
    #PBS -l select=2:ncpus=128:mpiprocs=128
```

> [!WARNING]
> **The maximum `nprocs` shown is an upper limit — not a recommendation.**
>
> This tool computes the *maximum* number of processors that avoids WRF domain decomposition errors (i.e., each tile has at least 10 grid points).  
> Using the maximum value does **not** necessarily yield optimal performance.
>
> See the linked presentation for an explanation of why increasing MPI ranks does not always improve performance.
>
> **Recommendations:**
> 1. Start with a value in the **middle** of the valid range  
> 2. Prefer **near-square decompositions** (e.g., `64 = 8×8`) over skinny layouts (e.g., `64 = 4×16`)  
> 3. Benchmark multiple configurations to find the performance sweet spot for your specific case

## Acknowledgement
This tool is dedicated to ([**Dave Gill**](https://github.com/davegill)), WRF original SE, and ([**Davide Del Vento**](https://github.com/ddvento)). When I was a grad student struggling to understand WRF's internals and domain decomposition, they took the time to explain things and patiently answer my many questions. If this tool makes WRF easier to use for someone else, then it's my small way of paying it forward.

## References

- [UCAR MMM Forum: Choosing an appropriate number of processors](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/)
- [WRF User's Guide](https://www2.mmm.ucar.edu/wrf/users/)
