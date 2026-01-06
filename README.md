# WRF Processor Configuration Tool

A tool to determine the optimal number of processors for WRF simulations and visualize domain decomposition.

## Overview

`wrf_num_procs.py` analyzes WRF domain configurations to determine:
- Minimum and maximum number of processors based on grid dimensions
- Optimal decomposition layout for parallel execution

The tool ensures each processor handles at least 10 grid points (the WRF minimum for adequate computation space beyond halo regions).

This script is adapted from a discussion on the [UCAR MMM Forum](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/).

## Background

### WRF Domain Decomposition

When running WRF in parallel, the domain is divided into tiles (one per processor). Each tile has:
- **Halo regions**: 5 rows/columns on each side for inter-processor communication
- **Computation space**: The interior region where actual calculations occur

To avoid tiles that are entirely halo regions, each tile must have at least **10 grid points per side**.

### Decomposition Strategy

The decomposition uses the two closest factors of the processor count to create near-square tile layouts:
- 16 processors -> 4x4 tiles (optimal)
- 11 processors -> 1x11 tiles (poor - prime number)

### Processor Bounds

For **nested domains**, processor limits are determined by:
- **Maximum processors**: Based on the **smallest** domain using `(e_we/10) * (e_sn/10)`
- **Minimum processors**: Based on the **largest** domain using `(e_we/100) * (e_sn/100)`

## Installation

No installation required. Just ensure Python 3 is available:

```bash
chmod +x wrf_num_procs.py
```

## Usage

### Basic Usage (Command Line)

```bash
./wrf_num_procs.py --e_we 320 --e_sn 180
```

### Multiple Domains

```bash
./wrf_num_procs.py --e_we 220 150 --e_sn 214 130
```

### Using Namelist File

```bash
./wrf_num_procs.py --namelist namelist.input
```

### With Decomposition Schematic

```bash
./wrf_num_procs.py --e_we 320 --e_sn 180 --decomp
```

## Example Output

### Single Domain

```
============================================================
  WRF Processor Configuration Analysis
============================================================
  Cores per node    : 128
  Min grid points   : 10 per processor
  Max grid points   : 100 per processor
  Domains to analyze: 1
    Domain 1: e_we = 320, e_sn = 180
============================================================

------------------------------------------------------------
  DOMAIN 1 of 1
------------------------------------------------------------
  Grid dimensions: 320 x 180 (57,600 total grid points)

  Processor range:
  Minimum.............      3 processors  =>  1 node @ 3 cores/node
  Maximum.............    256 processors  =>  2 nodes @ 128 cores/node

  Decomposition for 256 processors:
    Tile layout     : 16 x 16 = 256 tiles
    Grid per tile   : 20 x 11 points
    Remainder       : +4 in y
    Bottom edge     : 20x4 (16 tiles)

  Namelist settings:
    nproc_x = 16
    nproc_y = 16

============================================================
  Analysis complete
============================================================
```

### Nested Domains

```
============================================================
  WRF Processor Configuration Analysis
============================================================
  Cores per node    : 128
  Min grid points   : 10 per processor
  Max grid points   : 100 per processor
  Domains to analyze: 2
    Domain 1: e_we = 220, e_sn = 214
    Domain 2: e_we = 150, e_sn = 130
============================================================

------------------------------------------------------------
  DOMAIN 1 of 2
------------------------------------------------------------
  Grid dimensions: 220 x 214 (47,080 total grid points)

  Processor range:
  Minimum.............      4 processors  =>  1 node @ 4 cores/node
  Maximum.............    256 processors  =>  2 nodes @ 128 cores/node

  Decomposition for 256 processors:
    Tile layout     : 16 x 16 = 256 tiles
    Grid per tile   : 13 x 13 points
    Remainder       : +12 in x, +6 in y
    Right edge      : 12x13 (16 tiles)
    Bottom edge     : 13x6 (16 tiles)
    Corner          : 12x6 (1 tile)

  Namelist settings:
    nproc_x = 16
    nproc_y = 16

------------------------------------------------------------
  DOMAIN 2 of 2
------------------------------------------------------------
  Grid dimensions: 150 x 130 (19,500 total grid points)

  Processor range:
  Minimum.............      1 processors  =>  1 node @ 1 cores/node
  Maximum.............    121 processors  =>  1 node @ 121 cores/node

  Decomposition for 121 processors:
    Tile layout     : 11 x 11 = 121 tiles
    Grid per tile   : 13 x 11 points
    Remainder       : +7 in x, +9 in y
    Right edge      : 7x11 (11 tiles)
    Bottom edge     : 13x9 (11 tiles)
    Corner          : 7x9 (1 tile)

  Namelist settings:
    nproc_x = 11
    nproc_y = 11

============================================================
  Analysis complete
============================================================
```

## Command Line Arguments

| Argument | Description | Default |
|----------|-------------|---------|
| `--cores` | Number of cores per node | 128 |
| `--e_we` | Grid points in west-east direction (one or more values) | - |
| `--e_sn` | Grid points in south-north direction (one or more values) | - |
| `--namelist` | Path to WRF namelist file | - |
| `--decomp` | Show domain decomposition schematic | False |
| `--debug` | Enable debug output | False |

## Output Explanation

### Decomposition Information

- **Tile layout**: Number of tiles in x and y directions (nproc_x x nproc_y)
- **Grid per tile**: Base grid points per tile (e_we/nproc_x x e_sn/nproc_y)
- **Remainder**: Extra grid points distributed to edge tiles
- **Right edge**: Tile size for the rightmost column (has extra x points)
- **Bottom edge**: Tile size for the bottom row (has extra y points)
- **Corner**: Tile size for the bottom-right corner (has extra x and y points)

## Tips

1. **Avoid prime numbers** for processor counts - they result in 1xN decompositions
2. **Stay close to square** decompositions for better load balancing
3. **For nested domains**, use the smallest domain's max processors as your upper limit
4. **If simulations fail**, try using more processors (decomposition may be too coarse)

## References

- [UCAR MMM Forum: Choosing an appropriate number of processors](https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/)
- [WRF User's Guide](https://www2.mmm.ucar.edu/wrf/users/)
