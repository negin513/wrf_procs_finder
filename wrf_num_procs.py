#!/usr/bin/env python3
"""
WRF Processor Configuration Tool

A script to find the minimum and maximum number of processes for running WRF and plot schematic of domain decomposition.

`wrf_num_procs.py` determines the minimum and maximum number of processes and nodes based on the domain's grid points in the i/j directions.
It ensures that each processor handles at least a minimum number of grid points (i.e. 10 grids/tasks), optimizing computational resources for WRF simulations.

Usage:
    ./wrf_num_procs.py --e_we 320 --e_sn 180
    ./wrf_num_procs.py --e_we 220 150 --e_sn 214 130
    ./wrf_num_procs.py --namelist namelist.input
    ./wrf_num_procs.py --e_we 320 --e_sn 180 --decomp

Arguments:
    --cores      Number of cores per node (default: 128)
    --e_we       Grid points in west-east direction (one or more values)
    --e_sn       Grid points in south-north direction (one or more values)
    --namelist   Path to namelist file containing e_we, e_sn, and max_dom
    --decomp     Show domain decomposition schematic
    --ascii      Use ASCII borders in schematic (instead of Unicode box-drawing)
    --verbose    Show detailed remainder and edge tile information
    --debug      Enable debug mode for detailed output

WRF Domain Decomposition Background:
    The domain is divided into tiles (1 tile per proc). Each tile has 5 rows/columns
    of "halo" regions on each side for inter-processor communication. To ensure adequate
    computation space beyond halos, each tile must have at least 10 grid points per side.

    Decomposition uses the two closest factors to create near-square tile layouts.
    For example: 16 processors -> 4x4 tiles (good), 11 processors -> 1x11 (poor).

Processor Bounds for Nested Domains:
    - Max processors: Based on SMALLEST domain -> (e_we/10) * (e_sn/10)
    - Min processors: Based on LARGEST domain -> (e_we/100) * (e_sn/100)

    The script validates actual decomposition using MIN_GRID_POINTS=10, which may
    allow more processors than conservative formulas suggest.

Reference:
    https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/
"""

import argparse
import sys
import os
import math
import logging
import re


# Configuration Parameters
NODE_MAX = 200  # The maximum number of nodes to consider
MIN_GRID_POINTS = 10  # The minimum number of grid points per proc
MAX_GRID_POINTS = 100  # The maximum number of grid points per proc


def parse_arguments():
    """
    Parse command-line arguments.

    Returns:
        argparse.Namespace: The parsed arguments namespace.
    """
    parser = argparse.ArgumentParser(
        description="Determine the minimum and maximum number of processors and nodes based on number grid points."
    )

    # number of cores you want to use per node (e.g., 128 cores per node)
    parser.add_argument(
        "--cores",
        type=int,
        required=False,
        default=128,
        help="Number of cores per node (e.g., Derecho: 128 cores/node) [default = 128]",
    )
    parser.add_argument(
        "--e_we",
        type=int,
        nargs="+",  # Accept one or more integers
        help="e_we values if not using namelist",
    )
    parser.add_argument(
        "--e_sn",
        type=int,
        nargs="+",  # Accept one or more integers
        help="e_sn values if not using namelist",
    )
    parser.add_argument(
        "--namelist", type=str, help="Path to namelist file containing e_we and e_sn"
    )
    parser.add_argument(
        "--decomp",
        "--decomp_schematic",
        action="store_true",
        dest="decomp_schematic",
        help="Print domain decomposition schematic!",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug mode for detailed output",
    )
    parser.add_argument(
        "--ascii",
        action="store_true",
        help="Use ASCII characters for schematic borders (instead of Unicode)",
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Show detailed remainder and edge tile information",
    )

    return parser.parse_args()


def find_factors(n):
    """
    Find all factor pairs of a given number (here total number of processors).

    Args:
        n (int): total number of processors. (or the number to find the factors of)

    Returns:
        list of tuples: A list of tuples containing factor pairs.
    """
    factors = []
    for i in range(1, int(math.sqrt(n)) + 1):
        if n % i == 0:
            factors.append((i, n // i))
    return factors


def parse_namelist(namelist_path):
    """
    Parse the namelist file to extract e_we and e_sn values for multiple domains.

    The namelist file should contain lines in the format:
        e_we = 150,    220,
        e_sn = 130,    214,
        max_dom = 2,

    Args:
        namelist_path (str): Path to the namelist file.

    Returns:
        list of tuples: List containing (e_we, e_sn) for each domain up to max_dom.

    Raises:
        ValueError: If e_we, e_sn, or max_dom are not found or invalid.
    """
    if not os.path.isfile(namelist_path):
        raise ValueError(f"Namelist file '{namelist_path}' does not exist.")

    with open(namelist_path, "r") as file:
        content = file.read()

    # Remove comments (anything after '!')
    content = re.sub(r"!.*", "", content)

    # Define patterns for variables
    patterns = {
        "e_we": r"e_we\s*=\s*([\d,\s]+)",
        "e_sn": r"e_sn\s*=\s*([\d,\s]+)",
        "max_dom": r"max_dom\s*=\s*(\d+)",
    }

    # Extract variables using regex
    variables = {}
    for var, pattern in patterns.items():
        match = re.search(pattern, content, re.IGNORECASE)
        if not match:
            raise ValueError(f"'{var}' not found in the namelist file.")
        value = match.group(1)
        if var in ["e_we", "e_sn"]:
            # Convert comma-separated string to list of integers
            variables[var] = [
                int(num) for num in value.split(",") if num.strip().isdigit()
            ]
        else:
            variables[var] = int(value)

    e_we = variables["e_we"]
    e_sn = variables["e_sn"]
    max_dom = variables["max_dom"]

    # Validate that max_dom does not exceed available e_we and e_sn values
    if max_dom > len(e_we) or max_dom > len(e_sn):
        raise ValueError(
            f"max_dom={max_dom} exceeds the number of 'e_we' ({len(e_we)}) or 'e_sn' ({len(e_sn)}) values provided."
        )

    # Create list of (e_we, e_sn) pairs up to max_dom
    domain_pairs = list(zip(e_we, e_sn))[:max_dom]

    logging.debug(f"Domain pairs: {domain_pairs}")

    return domain_pairs


def calculate_processor_bounds(
    e_we,
    e_sn,
    cores_per_node,
    min_grid_points=MIN_GRID_POINTS,
    max_grid_points=MAX_GRID_POINTS,
    suggested_grid_point=25,
):
    """
    Calculate the maximum and minimum number of processors based on domain size.
    Additionally, calculate a suggested number of processors based on a recommended grid point.
    Compute the number of nodes required for the maximum, minimum, and suggested processors.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.
        cores_per_node (int): Number of cores per node.
        min_grid_points (int, optional): Minimum grid points per proc. Defaults to MIN_GRID_POINTS.
        max_grid_points (int, optional): Maximum grid points per proc. Defaults to MAX_GRID_POINTS.
        suggested_grid_point (int, optional): Suggested starting grid points per proc. Defaults to 24.

    Returns:
        tuple: (processors_min, processors_max, suggested_processors, suggested_nodes)
    """
    # Calculate maximum and minimum processors based on grid points
    processors_max = (e_we // min_grid_points) * (e_sn // min_grid_points)
    processors_min = max(1, (e_we // max_grid_points) * (e_sn // max_grid_points))

    # Calculate suggested number of processors based on suggested_grid_point
    suggested_processors = (e_we // suggested_grid_point) * (
        e_sn // suggested_grid_point
    )

    # Calculate number of nodes required for processors_max, processors_min, and suggested_processors
    nodes_max = math.ceil(processors_max / cores_per_node) if cores_per_node else 1
    nodes_min = math.ceil(processors_min / cores_per_node) if cores_per_node else 1
    suggested_nodes = (
        math.ceil(suggested_processors / cores_per_node) if cores_per_node else 1
    )

    # Debugging output
    logging.debug(f"Calculating processor bounds:")
    logging.debug(f"  e_we: {e_we}, e_sn: {e_sn}")
    logging.debug(
        f"  min_grid_points: {min_grid_points}, max_grid_points: {max_grid_points}"
    )
    logging.debug(
        f"  processors_min: {processors_min}, processors_max: {processors_max}"
    )
    logging.debug(f"  suggested_grid_point: {suggested_grid_point}")
    logging.debug(f"  suggested_processors: {suggested_processors}")
    logging.debug(
        f"  cores_per_node: {cores_per_node}, nodes_max: {nodes_max}, nodes_min: {nodes_min}, suggested_nodes: {suggested_nodes}"
    )

    # Print statements as per the specified format
    # print(
    #    f"Maximum number of processors: {processors_max} --> Requiring {nodes_max} nodes using {cores_per_node} procs/node"
    # )
    # print(
    #    f"Minimum number of processors: {processors_min} --> Requiring {nodes_min} nodes using {cores_per_node} procs/node"
    # )
    # print(f"Good starting point (per WRF docs):")
    # print(
    #    f"Number of processors: {suggested_processors} --> Requiring {suggested_nodes} nodes using {cores_per_node} procs/node"
    # )

    return processors_min, processors_max


def calculate_decomposition(e_we, e_sn, ntasks_x, ntasks_y):
    """
    Calculate the decomposition of the domain based on factor pairs.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.
        ntasks_x (int): Number of tasks for the i-direction.
        ntasks_y (int): Number of tasks for the j-direction.

    Returns:
        tuple: Decomposed grid points and remainders in i and j directions.

    """
    e_we_decomp = e_we // ntasks_x
    e_sn_decomp = e_sn // ntasks_y
    e_we_remainder = e_we % ntasks_x
    e_sn_remainder = e_sn % ntasks_y
    logging.debug(
        f"Decomposition: e_we_decomp={e_we_decomp}, e_sn_decomp={e_sn_decomp}, "
        f"e_we_remainder={e_we_remainder}, e_sn_remainder={e_sn_remainder}"
    )

    return e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder


def translate_procs_to_node(strategy, total_processors, cores_per_node=128):
    """
    Translate the total number of processors to the number of nodes required based on a strategy.

    Args:
        strategy (str): The strategy for allocation.
        total_processors (int): Total number of processors required.
        cores_per_node (int): Number of cores available per node (default is 128).

    Prints:
        str: A message describing the allocation.
    """
    effective_cores = min(total_processors, cores_per_node)
    num_nodes = math.ceil(total_processors / cores_per_node)

    node_word = "node" if num_nodes == 1 else "nodes"
    print(f"    {strategy:.<30} {total_processors:>6} procs  =>  {num_nodes} {node_word} @ {effective_cores} cores/node")


def print_domain_decomposition(
    max_procs,
    e_we_decomp,
    e_sn_decomp,
    e_we_remainder,
    e_sn_remainder,
    ntasks_x,
    ntasks_y,
    show_schematic=False,
    use_ascii=False,
    verbose=False,
):
    """
    Print a text representation of the domain decomposition.

    Args:
        max_procs (int): Total number of procs.
        e_we_decomp (int): Grid points per tile in the i-direction (x).
        e_sn_decomp (int): Grid points per tile in the j-direction (y).
        e_we_remainder (int): Remaining grid points in the i-direction.
        e_sn_remainder (int): Remaining grid points in the j-direction.
        ntasks_x (int): Number of tasks (tiles) in the i-direction.
        ntasks_y (int): Number of tasks (tiles) in the j-direction.
        show_schematic (bool): Whether to print the schematic.
        use_ascii (bool): Use ASCII characters for schematic borders.
        verbose (bool): Show detailed remainder and edge tile information.
    """

    print()
    print(f"  Decomposition for {max_procs} procs:")

    # Print out the tiles as a compact schematic
    if show_schematic:
        print()
        print_decomposition_schematic(
            ntasks_x, ntasks_y,
            e_we_decomp, e_sn_decomp,
            e_we_remainder, e_sn_remainder,
            use_ascii=use_ascii,
        )
        print()

    has_x_remainder = e_we_remainder > 0
    has_y_remainder = e_sn_remainder > 0

    # Default compact output
    print(f"      Layout    :  {ntasks_x:>3} x {ntasks_y:<3} tiles")
    print(f"      Avg tile  : ~{e_we_decomp:>3} x {e_sn_decomp:<3} grid points")

    # Verbose: show remainder info only if there are remainders
    if verbose and (has_x_remainder or has_y_remainder):
        print()
        print(f"      Remainder:")
        # Show edge and corner tile sizes
        if has_x_remainder and has_y_remainder:
            print(f"        Right edge:  {e_we_remainder:>3} x {e_sn_decomp:<3} ({ntasks_y} tiles)")
            print(f"        Botm edge :  {e_we_decomp:>3} x {e_sn_remainder:<3} ({ntasks_x} tiles)")
            print(f"        Corner    :  {e_we_remainder:>3} x {e_sn_remainder:<3} (1 tile)")
        elif has_x_remainder:
            print(f"        Right edge:  {e_we_remainder:>3} x {e_sn_decomp:<3} ({ntasks_y} tiles)")
        elif has_y_remainder:
            print(f"        Botm edge :  {e_we_decomp:>3} x {e_sn_remainder:<3} ({ntasks_x} tiles)")

    # Namelist settings (verbose only)
    if verbose:
        print()
        print(f"  Namelist settings:")
        print(f"    nproc_x = {ntasks_x}")
        print(f"    nproc_y = {ntasks_y}")


def print_decomposition_schematic(
    ntasks_x, ntasks_y,
    e_we_decomp, e_sn_decomp,
    e_we_remainder, e_sn_remainder,
    use_ascii=False,
):
    """
    Print a visual schematic of the domain decomposition.

    Shows all tiles in a grid with borders.

    Args:
        ntasks_x (int): Number of tiles in x-direction.
        ntasks_y (int): Number of tiles in y-direction.
        e_we_decomp (int): Grid points per tile in x-direction.
        e_sn_decomp (int): Grid points per tile in y-direction.
        e_we_remainder (int): Remainder grid points in x-direction.
        e_sn_remainder (int): Remainder grid points in y-direction.
        use_ascii (bool): Use ASCII characters instead of Unicode box-drawing.
    """
    # Box-drawing characters
    if use_ascii:
        H, V = "-", "|"
        TL, TR, BL, BR = "+", "+", "+", "+"
        TJ, BJ, LJ, RJ, X = "+", "+", "+", "+", "+"
    else:
        H, V = "─", "│"
        TL, TR, BL, BR = "┌", "┐", "└", "┘"
        TJ, BJ, LJ, RJ, X = "┬", "┴", "├", "┤", "┼"

    has_x_remainder = e_we_remainder > 0
    has_y_remainder = e_sn_remainder > 0

    # Build tile size strings
    base_tile = f"{e_we_decomp}x{e_sn_decomp}"
    x_rem_tile = f"{e_we_remainder}x{e_sn_decomp}" if has_x_remainder else ""
    y_rem_tile = f"{e_we_decomp}x{e_sn_remainder}" if has_y_remainder else ""
    corner_tile = f"{e_we_remainder}x{e_sn_remainder}" if has_x_remainder and has_y_remainder else ""

    # Calculate max width for alignment (add padding)
    all_tiles = [base_tile]
    if x_rem_tile:
        all_tiles.append(x_rem_tile)
    if y_rem_tile:
        all_tiles.append(y_rem_tile)
    if corner_tile:
        all_tiles.append(corner_tile)
    cell_width = max(len(t) for t in all_tiles) + 2  # +2 for padding

    # Determine column count
    num_cols = ntasks_x + (1 if has_x_remainder else 0)

    # Build horizontal separator lines
    def make_hsep(left, mid, right, fill):
        segments = [fill * cell_width] * num_cols
        return "    " + left + mid.join(segments) + right

    top_border = make_hsep(TL, TJ, TR, H)
    mid_border = make_hsep(LJ, X, RJ, H)
    bot_border = make_hsep(BL, BJ, BR, H)

    # Build a data row
    def make_row(tiles):
        cells = [t.center(cell_width) for t in tiles]
        return "    " + V + V.join(cells) + V

    # Determine total rows
    total_rows = ntasks_y + (1 if has_y_remainder else 0)

    # Print the schematic
    print(top_border)
    for row_idx in range(total_rows):
        is_remainder_row = has_y_remainder and row_idx == total_rows - 1

        if is_remainder_row:
            tiles = [y_rem_tile] * ntasks_x
            if has_x_remainder:
                tiles.append(corner_tile)
        else:
            tiles = [base_tile] * ntasks_x
            if has_x_remainder:
                tiles.append(x_rem_tile)

        print(make_row(tiles))

        if row_idx < total_rows - 1:
            print(mid_border)

    print(bot_border)


def find_max_processors(
    e_we,
    e_sn,
    cores_per_node,
    node_max,
    min_grid_points,
):
    """
    Determine the maximum number of processors and nodes that can be used.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.
        cores_per_node (int): Number of cores per node.
        node_max (int): Maximum number of nodes to consider.
        min_grid_points (int): Minimum grid points per proc.

    Returns:
        tuple: Maximum processors and nodes.
    """
    original_cores = cores_per_node

    for node_count in range(1, node_max + 1):
        total_tasks = original_cores * node_count
        factors = find_factors(total_tasks)

        if not factors:
            logging.debug(f"No factor pairs found for {total_tasks} cores.")
            continue

        # Select the factor pair closest to a square configuration
        closest_factors = min(factors, key=lambda x: abs(x[0] - x[1]))

        # of the closest factor pairs, assign the i and j values
        ntasks_x, ntasks_y = closest_factors

        # Calculate the decomposition of the domain based on the factor pairs
        e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder = (
            calculate_decomposition(e_we, e_sn, ntasks_x, ntasks_y)
        )
        # print_domain_decomposition(
        #    e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder, ntasks_x, ntasks_y
        # )
        logging.debug(f"Testing Number of Nodes: {node_count} --> ")
        logging.debug(
            f"  Number of tasks in x: {ntasks_x}, Number of tasks in y: {ntasks_y}"
        )
        logging.debug(f"  Each Tile has: ")
        logging.debug(
            f"  num grids in we: {e_we_decomp}, num grids in sn: {e_sn_decomp}"
        )
        logging.debug(f"  Remainder: ")
        logging.debug(
            f"  num grids in we: {e_we_remainder}, num grids in sn: {e_sn_remainder}"
        )

        # Check if the decomposition meets the minimum grid points requirement
        # Once the decomposition becomes smaller than the least number of grid points
        # allowed for each processor, the loop will quit and display the max
        # number of processors and nodes you can use for your domain.
        logging.debug(
            f"  e_we = {e_we}, nproc_x = {ntasks_x}, with cell width in x-direction = {e_we_decomp}"
        )
        logging.debug(
            f"  e_sn = {e_sn}, nproc_y = {ntasks_y}, with cell width in y-direction = {e_sn_decomp}"
        )

        # If decomposition is too small, try reducing the number of processors
        if e_we_decomp < min_grid_points or e_sn_decomp < min_grid_points:
            # test to see if the max number of processors allowed is within the number for a single node
            logging.debug(
                "Decomposition is too small! Trying to reduce the number of processors: "
            )
            initial_factor_pair = factors[0]
            initial_factor = initial_factor_pair[1]
            logging.debug(
                f"Initial factor pair: {initial_factor_pair}, initial_factor={initial_factor}"
            )

            # If the initial factor equals the original number of cores, adjust processors per node
            if initial_factor == original_cores:

                # start with value of cores_orig and decrease by 1 for each iteration
                # until the value is allowed
                y = original_cores
                # back sweep to find the largest number of processors that can be used
                while y >= 1:
                    processors = y
                    logging.debug(f"Trying with {processors} processors")

                    # Find factor pairs for the processors
                    reduced_factors = find_factors(processors)
                    logging.debug(
                        f"Trying with {processors} processors: {reduced_factors}."
                    )

                    if not reduced_factors:
                        y -= 1
                        continue

                    # Of the factor pairs, find the closest to square
                    closest_reduced_factors = min(
                        reduced_factors, key=lambda x: abs(x[0] - x[1])
                    )

                    i_reduced, j_reduced = closest_reduced_factors
                    logging.debug(f"Reduced factors: {closest_reduced_factors}")
                    logging.debug(
                        f"Selected reduced factor pair: {closest_reduced_factors}"
                    )

                    # Calculate how the domain will be decomposed
                    # still testing processor values for a single node

                    e_we_reduced, e_sn_reduced, e_we_remainder, e_sn_remainder = (
                        calculate_decomposition(e_we, e_sn, i_reduced, j_reduced)
                    )

                    # Once the decomposition becomes larger or equal to the least number of grid points
                    # allowed for each processor, the loop will quit and display the max
                    # number of processors and nodes you can use for your domain.
                    if (e_we_reduced >= min_grid_points) and (
                        e_sn_reduced >= min_grid_points
                    ):
                        max_procs = i_reduced * j_reduced
                        return max_procs, 1

                    # if you haven't reached your limit, the loop continues
                    # still testing processor values for a single node
                    else:
                        y -= 1

            # if the size of the domain allows multiple nodes
            else:
                max_procs = ntasks_x * ntasks_y - original_cores
                max_nodes = math.ceil(max_procs / original_cores)
                logging.debug(
                    f"Multiple node configuration: max_procs={max_procs}, max_nodes={max_nodes}"
                )
                return max_procs, max_nodes

            # After adjustment, break the loop
            break

    # If no suitable configuration is found within bounds
    return 0, 0


def main():
    args = parse_arguments()

    # Configure logging
    if args.debug:
        logging.basicConfig(level=logging.DEBUG, format="%(levelname)s: %(message)s")
        logging.debug("Debug mode is enabled.")
    else:
        logging.basicConfig(level=logging.INFO, format="%(message)s")

    try:
        if args.namelist:
            domain_pairs = parse_namelist(args.namelist)

        elif args.e_we and args.e_sn:
            if len(args.e_we) != len(args.e_sn):
                raise ValueError("The number of e_we and e_sn values must be the same.")
            domain_pairs = list(zip(args.e_we, args.e_sn))
        else:
            # If only one of e_we or e_sn is provided without namelist
            print(
                "Error: When not using --namelist, both --e_we and --e_sn must be provided.",
                file=sys.stderr,
            )
            sys.exit(1)
    except ValueError as ve:
        print(f"Error: {ve}", file=sys.stderr)
        sys.exit(1)

    # Print header (verbose only)
    total_domains = len(domain_pairs)
    print()
    if args.verbose:
        print("=" * 60)
        print("  WRF Domain Decomposition Analysis")
        print("=" * 60)
        print(f"  Cores per node    : {args.cores}")
        print(f"  Min grid points   : {MIN_GRID_POINTS} per proc")
        print(f"  Max grid points   : {MAX_GRID_POINTS} per proc")
        print(f"  Domains to analyze: {total_domains}")
        for idx, (e_we, e_sn) in enumerate(domain_pairs):
            print(f"    Domain {idx + 1}: e_we = {e_we}, e_sn = {e_sn}")
        print("=" * 60)

    # Track min/max across all domains for combined summary
    all_mins = []
    all_maxs = []

    for idx, (e_we, e_sn) in enumerate(domain_pairs):
        print()
        print("-" * 60)
        print(f"  DOMAIN {idx + 1} of {total_domains}")
        print("-" * 60)
        print(f"  e_we = {e_we}")
        print(f"  e_sn = {e_sn}")
        print(f"  Total grid points: {e_we} x {e_sn} = {e_we * e_sn:,}")
        print()
        print("  nprocs range:")

        # Calculate maximum and minimum processors based on domain size
        processors_min, processors_max = calculate_processor_bounds(
            e_we, e_sn, args.cores
        )
        translate_procs_to_node("Minimum", processors_min, args.cores)

        # Determine max processors and nodes based on decomposition logic within bounds
        max_procs, max_nodes = find_max_processors(
            e_we,
            e_sn,
            cores_per_node=args.cores,
            node_max=NODE_MAX,
            min_grid_points=MIN_GRID_POINTS,
        )

        if max_procs > 0:
            factors = find_factors(max_procs)
            closest_factors = min(factors, key=lambda x: abs(x[0] - x[1]))
            ntasks_x, ntasks_y = closest_factors
            e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder = (
                calculate_decomposition(e_we, e_sn, ntasks_x, ntasks_y)
            )
            translate_procs_to_node("Maximum", max_procs, args.cores)
            print_domain_decomposition(
                max_procs,
                e_we_decomp,
                e_sn_decomp,
                e_we_remainder,
                e_sn_remainder,
                ntasks_x,
                ntasks_y,
                args.decomp_schematic,
                args.ascii,
                args.verbose,
            )

        # Track for combined summary
        all_mins.append(max(1, processors_min))
        all_maxs.append(max_procs if max_procs > 0 else processors_max)

    # Print combined summary for multiple domains
    if total_domains > 1:
        print()
        print("=" * 60)
        print("  COMBINED NPROCS RANGE (all domains)")
        print("=" * 60)
        combined_min = max(all_mins)  # Limited by largest domain
        combined_max = min(all_maxs)  # Limited by smallest domain

        if combined_min > combined_max:
            print()
            print("  WARNING: No valid nprocs range exists!")
            print("  The domains vary too much in size.")
            print("  Consider using ndown to run domains separately.")
        else:
            print("  nprocs range:")
            translate_procs_to_node("Minimum (largest domain)", combined_min, args.cores)
            translate_procs_to_node("Maximum (smallest domain)", combined_max, args.cores)

    print()


if __name__ == "__main__":
    main()
