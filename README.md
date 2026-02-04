[![License](https://img.shields.io/badge/License-Apache_2.0-blue.svg)](https://opensource.org/licenses/Apache-2.0)

# WRF Processor Configuration Tool

A tool to determine the valid processor range for WRF simulations and visualize domain decomposition.
Matches WRF's actual validation logic from `share/module_check_a_mundo.F`.

## Overview

`wrf_num_procs.py` analyzes WRF domain configurations to determine:
- Minimum and maximum number of processors based on grid dimensions
- Decomposition layout for parallel execution
- Node requirements based on procs per node

The tool ensures each processor handles at least 10 grid points (the WRF minimum for adequate computation space beyond halo regions).

This script is adapted from a discussion on the [UCAR MMM Forum](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/).

## Quick Start

```bash
# Clone and enter directory
git clone https://github.com/negin513/wrf_procs_finder.git
cd wrf_procs_finder

# From a namelist file
./wrf_num_procs.py --namelist examples/namelist.input

# From grid dimensions directly
./wrf_num_procs.py --e_we 320 --e_sn 180

# With visual decomposition schematic
./wrf_num_procs.py --namelist examples/namelist.input --decomp
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

### Decomposition Strategy

WRF decomposition uses the two closest factors of the processor count to create near-square tile layouts:
- 16 processors → 4×4 tiles (good)
- 11 processors → 1×11 tiles (poor - prime number)

### Processor Bounds for Nested Domains

- **Maximum processors**: Based on the **smallest** domain → `(e_we/10) * (e_sn/10)`
- **Minimum processors**: Based on the **largest** domain → `(e_we/100) * (e_sn/100)`

## Example Output

Running with the example namelist (`./wrf_num_procs.py --namelist examples/namelist.input`):

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
  Decomposition for 128 procs:

    ┌───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┐
    │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │ 18x8  │
    ├───────┼───────┼───────┼───────┼───────┼───────┼───────┼───────┤
    ...
    └───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┘

      Layout    :    8 x 16  tiles
      Avg tile  :  ~18 x 8   grid points
```

### Warning for Incompatible Domains

If domains vary too much in size, the valid ranges may not overlap:

```
  WARNING: No valid nprocs range exists!
  The domains vary too much in size.
  Consider using ndown to run domains separately.
```

## Command Line Arguments

| Argument | Description | Default |
|----------|-------------|---------|
| `--cores` | Number of cores per node | 128 |
| `--e_we` | Grid points in west-east direction (one or more values) | - |
| `--e_sn` | Grid points in south-north direction (one or more values) | - |
| `--namelist` | Path to WRF namelist file | - |
| `--decomp` | Show domain decomposition schematic | False |
| `--ascii` | Use ASCII characters for schematic borders | False |
| `--verbose`, `-v` | Show detailed output (remainders, namelist settings) | False |
| `--quiet`, `-q` | Suppress notes and warnings | False |
| `--debug` | Enable debug output | False |

## Derecho Support

When running on NCAR's Derecho supercomputer, the tool automatically generates PBS select lines:

```
  Derecho PBS (for max procs):
    #PBS -l select=2:ncpus=128:mpiprocs=128
```

## Tips

1. **Don't use the maximum** - It's an upper limit, not a recommendation. Start in the middle of the valid range and benchmark.
2. **Avoid prime numbers** for processor counts - they result in 1×N decompositions with poor communication patterns.
3. **Prefer square decompositions** - 8×8 is better than 4×16 for the same processor count.
4. **For nested domains**, use the combined range shown at the end.

## References

- [UCAR MMM Forum: Choosing an appropriate number of processors](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/)
- [WRF User's Guide](https://www2.mmm.ucar.edu/wrf/users/)
