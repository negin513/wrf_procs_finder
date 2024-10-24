#!/usr/bin/env python3
"""
This script determines the minimum and maximum number of processors and nodes 
that can be used based on the domain's grid points in the i/j directions.
It ensures that each processor handles at least a minimum number of grid points.

The script is adapted from this post: 
https://forum.mmm.ucar.edu/threads/choosing-an-appropriate-number-of-processors.5082/

Usage:
    python find_max_procs_nodes.py --cores <cores_per_node> --e_we <e_we> --e_sn <e_sn>
    python find_max_procs_nodes.py --cores <cores_per_node> --namelist <namelist_file>

Arguments:
    --cores      Number of cores per node (e.g., 128)
    --e_we       Number of grid points in the i-direction
    --e_sn       Number of grid points in the j-direction
    --namelist   Path to namelist file containing e_we and e_sn

Example:
    ./find_wrf_procs.py --cores 128 --e_we 1368 --e_sn 1016
    ./find_wrf_procs.py --cores 128 --namelist namelist.input
"""

import argparse
from re import S
import sys
import os
import math


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
    Parse the namelist file to extract e_we and e_sn values.

    The namelist file should contain lines in the format:
        e_we = 1368
        e_sn = 1016

    Args:
        namelist_path (str): Path to the namelist file.

    Returns:
        tuple: e_we and e_sn values.

    Raises:
        ValueError: If e_we or e_sn are not found or invalid.
    """
    if not os.path.isfile(namelist_path):
        raise ValueError(f"Namelist file '{namelist_path}' does not exist.")

    e_we = e_sn = None
    with open(namelist_path, "r") as file:
        for line in file:
            line = line.strip()
            if line.startswith("e_we"):
                try:
                    e_we = int(line.split("=")[1].strip().rstrip(","))
                except (IndexError, ValueError):
                    raise ValueError("Invalid format for 'e_we' in namelist file.")
            elif line.startswith("e_sn"):
                try:
                    e_sn = int(line.split("=")[1].strip().rstrip(","))
                except (IndexError, ValueError):
                    raise ValueError("Invalid format for 'e_sn' in namelist file.")

    if e_we is None or e_sn is None:
        raise ValueError("Namelist file must contain both 'e_we' and 'e_sn' values.")

    return e_we, e_sn


def calculate_processor_bounds(e_we, e_sn, min_grid_points=MIN_GRID_POINTS, max_grid_points=MAX_GRID_POINTS):
    """
    Calculate the maximum and minimum number of processors based on domain size.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.

    Returns:
        tuple: (processors_min, processors_max)
    """
    processors_max = (e_we // min_grid_points) * (e_sn // min_grid_points)
    processors_min = (e_we // max_grid_points) * (e_sn // max_grid_points)
    print(f"processors_min: {processors_min}, processors_max: {processors_max}")
    return processors_min, processors_max


def calculate_decomposition(e_we, e_sn, ntasks_x, ntasks_y):
    """
    Calculate the decomposition of the domain based on factor pairs.

    Args:
        e_we (int): Grid points in the i-direction.
        e_sn (int): Grid points in the j-direction.
        ntasks_x (int): Factor for the i-direction.
        ntasks_y (int): Factor for the j-direction.

    Returns:
        tuple: Decomposed grid points in i and j directions.
    """
    e_we_decomp = e_we // ntasks_x
    e_sn_decomp = e_sn // ntasks_y
    e_we_remainder = e_we % ntasks_x
    e_sn_remainder = e_sn % ntasks_y

    return e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder


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
            continue
        
        # Select the factor pair closest to a square configuration; hint: the last one
        closest_factors = factors[-1]

        # of the closest factor pairs, assign the i and j values
        ntasks_x, ntasks_y = closest_factors

        # Calculate the decomposition of the domain based on the factor pairs
        e_we_decomp, e_sn_decomp, e_we_remainder, e_sn_remainder = (
            calculate_decomposition(e_we, e_sn, ntasks_x, ntasks_y)
        )
        print("----------------")
        print(f"Testing Number of Nodes: {node_count} --> ")
        print(f"  Number of tasks in x: {ntasks_x}, Number of tasks in y: {ntasks_y}")
        print(f"  Each Tile has: ")
        print(f"  num grids in we: {e_we_decomp}, num grids in sn: {e_sn_decomp}")
        print(f"  Remainder: ")
        print(f"  num grids in we: {e_we_remainder}, num grids in sn: {e_sn_remainder}")

        # Check if the decomposition meets the minimum grid points requirement
        # Once the decomposition becomes smaller than the least number of grid points
        # allowed for each processor, the loop will quit and display the max 
        # number of processors and nodes you can use for your domain.

        # If decomposition is too small, try reducing the number of processors
        if e_we_decomp < min_grid_points or e_sn_decomp < min_grid_points:
            # test to see if the max number of processors allowed is within the number for a single node 

            print("**************")
            print(
                "Decomposition is too small! Trying to reduce the number of processors: "
            )
            initial_factor_pair = factors[0]
            initial_factor = initial_factor_pair[1]

            print(initial_factor_pair)

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
                    if not reduced_factors:
                        y -= 1
                        continue

                    # Of the factor pairs, this finds the closest values (pair) in that array
                    # still testing processor values for a single node
                    
                    closest_reduced_factors = reduced_factors[-1]
                    i_reduced, j_reduced = closest_reduced_factors
                    print(f"Reduced factors: {closest_reduced_factors}")

                    # Calculate how the domain will be decomposed
                    # still testing processor values for a single node

                    e_we_reduced, e_sn_reduced, e_we_remainder, e_sn_remainder = (
                        calculate_decomposition(e_we, e_sn, i_reduced, j_reduced)
                    )

                    # Once the decomposition becomes larger or equal to the least number of grid points
                    # allowed for each processor, the loop will quit and display the max 
                    # number of processors and nodes you can use for your domain.
                    # If valid decomposition, ensure it's within processors count bounds
                    if (e_we_reduced >= min_grid_points) and (e_sn_reduced >= min_grid_points):
                        max_procs = i_reduced * j_reduced

                        if processors_min <= max_procs <= processors_max:
                            print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                            print(
                                "Note: Adjusted processor count to fit within bounds."
                            )
                            print(f"Max # of nodes that can be used: 1")
                            print(f"Max # of processors that can be used: {max_procs}")
                            print(
                                f"Number of tasks in x: {i_reduced}, Number of tasks in y: {j_reduced}"
                            )
                            print(f"Each Tile has: ")
                            print(
                                f"num grids in we: {e_we_reduced}, num grids in sn: {e_sn_reduced}"
                            )
                            print(f"Remainder tile: ")
                            print(
                                f"num grids in we: {e_we_remainder}, num grids in sn: {e_sn_remainder}"
                            )
                            print("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
                            return max_procs, 1

                    # if you haven't reached your limit, the loop continues
                    # still testing processor values for a single node                               
                    else:
                        y -= 1

            # if the size of the domain allows multiple nodes
            else:
                max_procs = ntasks_x * ntasks_y
                max_nodes = math.ceil(max_procs / original_cores)
                print("&&&&&&&&&&&&")
                print ("max # of processors that can be used is: ", max_procs)
                print ("max # of nodes that can be used is: ", max_nodes)

                print ("min processors: ", processors_min)
                print ("max processors: ", processors_max)

                # Ensure that max_procs is within the [processors_min, processors_max] range
                if processors_min <= max_procs <= processors_max:
                    return max_procs, max_nodes
                 # Ensure that max_procs does not exceed processors_max
                elif max_procs > processors_max:
                    print ("did not meet the processors_max condition")
                    max_procs = processors_max
                    max_nodes = math.ceil(max_procs / original_cores)
                    return max_procs, max_nodes
            
            # After adjustment, break the loop
            break

    # If no suitable configuration is found within bounds
    return 0, 0


def main():
    args = parse_arguments()
    n = 128 * 23
    khar = find_factors(n)
    print(khar)
    print("=====")



    try:
        if args.namelist:
            e_we, e_sn = parse_namelist(args.namelist)
        elif args.e_we and args.e_sn:
            e_we, e_sn = args.e_we, args.e_sn
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

    # Calculate maximum and minimum processors based on domain size
    processors_min, processors_max = calculate_processor_bounds(e_we, e_sn)

    print(
        f"Maximum number of processors based on smallest-sized domain: {processors_max}"
    )
    print(
        f"Minimum number of processors based on largest-sized domain: {processors_min}"
    )

    # Determine max processors and nodes based on decomposition logic within bounds
    max_processors, max_nodes = find_max_processors(
        e_we,
        e_sn,
        args.cores,
        NODE_MAX,
        MIN_GRID_POINTS,
        processors_min,
        processors_max,
    )

    if max_processors == 0:
        print("No suitable processor and node configuration found.")
    else:
        print(f"Max # of processors that can be used: {max_processors}")
        print(f"Max # of nodes that can be used: {max_nodes}")


if __name__ == "__main__":
    main()
