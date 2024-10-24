#!/usr/bin/env python3
"""
This script determines the minimum and maximum number of processors and nodes 
that can be used based on the domain's grid points in the i/j directions.
It ensures that each processor handles at least a minimum number of grid points.

The script is adapted from this post: 
https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/

Arguments:
    --cores      Number of cores per node (e.g., 128)
    --e_we       Number of grid points in the i-direction
    --e_sn       Number of grid points in the j-direction
    --namelist   Path to namelist file containing e_we and e_sn
    --debug      Enable debug mode for detailed output


Example:
    ./find_wrf_procs.py --cores 128 --e_we 1368 --e_sn 1016
    ./find_wrf_procs.py --cores 128 --namelist namelist.input
"""

import argparse
import sys
import os
import math
import logging
import re


# Configuration Parameters
NODE_MAX = 200  # The maximum number of nodes to consider
MIN_GRID_POINTS = 10  # The minimum number of grid points per processor
MAX_GRID_POINTS = 100  # The maximum number of grid points per processor


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
        "--e_we", type=int, help="Number of grid points in the i-direction"
    )
    parser.add_argument(
        "--e_sn", type=int, help="Number of grid points in the j-direction"
    )
    parser.add_argument(
        "--namelist", type=str, help="Path to namelist file containing e_we and e_sn"
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="Enable debug mode for detailed output",
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

    print(domain_pairs)

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
        min_grid_points (int, optional): Minimum grid points per processor. Defaults to MIN_GRID_POINTS.
        max_grid_points (int, optional): Maximum grid points per processor. Defaults to MAX_GRID_POINTS.
        suggested_grid_point (int, optional): Suggested starting grid points per processor. Defaults to 24.

    Returns:
        tuple: (processors_min, processors_max, suggested_processors, suggested_nodes)
    """
    # Calculate maximum and minimum processors based on grid points
    processors_max = (e_we // min_grid_points) * (e_sn // min_grid_points)
    processors_min = (e_we // max_grid_points) * (e_sn // max_grid_points)

    # Calculate suggested number of processors based on suggested_grid_point
    suggested_processors = (e_we // suggested_grid_point) * (
        e_sn // suggested_grid_point
    )

    # Calculate number of nodes required for processors_max, processors_min, and suggested_processors
    nodes_max = math.ceil(processors_max / cores_per_node) if cores_per_node else 0
    nodes_min = math.ceil(processors_min / cores_per_node) if cores_per_node else 0
    suggested_nodes = (
        math.ceil(suggested_processors / cores_per_node) if cores_per_node else 0
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


def print_domain_decomposition(
    max_procs,
    e_we_decomp,
    e_sn_decomp,
    e_we_remainder,
    e_sn_remainder,
    ntasks_x,
    ntasks_y,
    show_schematic=False,
):
    """
    Print a text representation of the domain decomposition.

    Args:
        max_procs (int): Total number of processors.
        e_we_decomp (int): Grid points per tile in the i-direction (x).
        e_sn_decomp (int): Grid points per tile in the j-direction (y).
        e_we_remainder (int): Remaining grid points in the i-direction.
        e_sn_remainder (int): Remaining grid points in the j-direction.
        ntasks_x (int): Number of tasks (tiles) in the i-direction.
        ntasks_y (int): Number of tasks (tiles) in the j-direction.
        show_schematic (bool): Whether to print the schematic.
    """
    if not show_schematic:
        return

    # Build the base tile representation
    base_tile = f"{e_we_decomp}x{e_sn_decomp}"
    remainder_tile_x = f"{e_we_remainder}x{e_sn_decomp}" if e_we_remainder > 0 else ""
    remainder_tile_y = f"{e_we_decomp}x{e_sn_remainder}" if e_sn_remainder > 0 else ""
    remainder_tile_corner = (
        f"{e_we_remainder}x{e_sn_remainder}"
        if e_we_remainder > 0 and e_sn_remainder > 0
        else ""
    )

    print("-" * 40)
    print(f"Domain Decomposition Layout with {max_procs} Processes:")

    # Print out the tiles row by row
    for y in range(ntasks_y):
        row = " | ".join([base_tile] * ntasks_x)
        if remainder_tile_x:
            row += " | " + remainder_tile_x
        print(row)

    # Print the remainder row if needed
    if remainder_tile_y:
        row = " | ".join([remainder_tile_y] * ntasks_x)
        if remainder_tile_corner:
            row += " | " + remainder_tile_corner
        print(row)

    print("-" * 40)

    print("Summary:")
    print(
        f"  Base tile size : {base_tile} (each tile has {e_we_decomp} x {e_sn_decomp} grid points)"
    )
    print(f"  Total tiles    : {ntasks_x} x {ntasks_y}")
    if e_we_remainder > 0:
        print(
            f"  Extra column with tile size : {e_we_remainder} x {e_sn_decomp} (added to the right)"
        )
    if e_sn_remainder > 0:
        print(
            f"  Extra row with tile size    : {e_we_decomp} x {e_sn_remainder} (added to the bottom)"
        )
    if remainder_tile_corner:
        print(
            f"  Extra corner tile size      : {remainder_tile_corner} (added at the bottom-right corner)"
        )
    print("-" * 40)




def find_max_processors_with_print(*args, **kwargs):
    max_procs, max_nodes = find_max_processors(*args, **kwargs)
    cores_per_node = kwargs.get("cores", 128)

    if max_procs > 0:
        # Recompute decomposition for printing
        factors = find_factors(max_procs)
        closest_factors = min(factors, key=lambda x: abs(x[0] - x[1]))
        ntasks_x, ntasks_y = closest_factors
        e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder = (
            calculate_decomposition(args[0], args[1], ntasks_x, ntasks_y)
        )
        translate_procs_to_node("Maximum", max_procs, cores_per_node)
        print_domain_decomposition(
            max_procs,
            e_we_decomp,
            e_sn_decomp,
            e_we_remainder,
            e_sn_remainder,
            ntasks_x,
            ntasks_y,
        )
    return max_procs, max_nodes


def translate_procs_to_node(strategy, total_processors, cores_per_node=128):
    """
    Translate the total number of processors to the number of nodes required based on a strategy.

    Args:
        strategy (str): The strategy for allocation.
        total_processors (int): Total number of processors required.
        num_nodes (int): The number of nodes to use.
        cores_per_node (int): Number of cores available per node (default is 128).

    Prints:
        str: A message describing the allocation.
    """
    cores_per_node = min(total_processors, cores_per_node)
    num_nodes = math.ceil(total_processors / cores_per_node)
    message = (
        f"{strategy} "
        f"number of processors: {total_processors} --> Requiring {num_nodes} nodes "
        f"using ({cores_per_node} procs/node)"
    )
    print(message)


def find_max_processors(
    e_we,
    e_sn,
    cores_per_node,
    node_max,
    min_grid_points,
    processors_min,
    processors_max,
):
    """
    Determine the maximum number of processors and nodes that can be used.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.
        cores_per_node (int): Number of cores per node.
        node_max (int): Maximum number of nodes to consider.
        min_grid_points (int): Minimum grid points per processor.
        processors_min (int): Minimum number of processors based on domain size.
        processors_max (int): Maximum number of processors based on domain size.

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

        # Select the factor pair closest to a square configuration; hint: the last one
        # closest_factors = factors[-1]
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
            print("......")
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
                    print(f"Trying with {processors} processors")

                    # Find factor pairs for the processors
                    reduced_factors = find_factors(processors)
                    logging.debug(
                        f"Trying with {processors} processors: {reduced_factors}."
                    )

                    if not reduced_factors:
                        y -= 1
                        continue

                    # Of the factor pairs, this finds the closest values (pair) in that array
                    # still testing processor values for a single node

                    # closest_reduced_factors = reduced_factors[-1]
                    closest_reduced_factors = min(
                        reduced_factors, key=lambda x: abs(x[0] - x[1])
                    )

                    i_reduced, j_reduced = closest_reduced_factors
                    print(f"Reduced factors: {closest_reduced_factors}")
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
                    # If valid decomposition, ensure it's within processors count bounds
                    if (e_we_reduced >= min_grid_points) and (
                        e_sn_reduced >= min_grid_points
                    ):
                        max_procs = i_reduced * j_reduced

                        # This check is redundant, but it's here to ensure the processor count is within bounds!
                        if processors_min <= max_procs <= processors_max:
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

                # Ensure that max_procs is within the [processors_min, processors_max] range
                if processors_min <= max_procs <= processors_max:
                    return max_procs, max_nodes
                # Ensure that max_procs does not exceed processors_max
                elif max_procs > processors_max:
                    max_procs = processors_max
                    max_nodes = math.ceil(max_procs / original_cores)
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

    for idx, (e_we, e_sn) in enumerate(domain_pairs):
        print("=" * 40)
        print(f"Domain {idx + 1}: e_we={e_we}, e_sn={e_sn}")

        # Calculate maximum and minimum processors based on domain size
        processors_min, processors_max = calculate_processor_bounds(
            e_we, e_sn, args.cores
        )
        translate_procs_to_node("Minimum", processors_min, args.cores)

        cores_per_node = args.cores
        # Determine max processors and nodes based on decomposition logic within bounds
        max_procs, max_nodes = find_max_processors_with_print(
            e_we,
            e_sn,
            cores_per_node,
            NODE_MAX,
            MIN_GRID_POINTS,
            processors_min,
            processors_max,
        )


if __name__ == "__main__":
    main()
