# This script finds the largest number of processors and nodes
# you can use, based on the number of grid points in the i/j directions
# on your domain. 

# Note: The largest number may not decompose in the best way. 
# If you want additional values, set some print statements in the code below.

# enter the namelist values of e_we and e_sn
e_we = 1368
e_sn = 1016

# number of cores you want to use per node (e.g., 128 cores per node)
cores = 128

# The value for 'cores' gets incremented later, so we want a static variable for the original value 
cores_orig = cores

# set upper limit of nodes - the max you want to loop through
node_max = 200

# This is the least number of grid points allowed for each processor. 
smallest_size = 25

# Function to find factor pairs for a given number
def find_factors(n):
    factors = []
    for i in range(1, int(n**0.5) + 1):
        if n % i == 0:
            factors.append((i, n // i))  # Use // for integer division
    return factors

x = 1
while x <= node_max:
    # Find factor pairs for the current number of cores
    factors = find_factors(cores)

    # Find the closest factor pair (last element in the list of factors)
    closest_factors = factors[-1]

    # Assign the i and j values (the closest factor pair)
    i_array_value = closest_factors[0]
    j_array_value = closest_factors[1]

    # Calculate how the domain will be decomposed
    e_we_decomp = e_we // i_array_value
    e_sn_decomp = e_sn // j_array_value

    # Check if the decomposition is smaller than the least number of grid points allowed per processor
    if e_we_decomp < smallest_size or e_sn_decomp < smallest_size:
        # Test to see if the number of processors is within the range for a single node
        initial_factor_pair = factors[0]
        initial_factor = initial_factor_pair[1]

        # If the initial factor equals the original number of cores, we adjust processors per node
        if initial_factor == cores_orig:
            y = cores_orig
            while y >= 1:
                processors = y

                # Find factor pairs for the processors
                factors = find_factors(processors)
                closest_factors = factors[-1]

                i_array_value = closest_factors[0]
                j_array_value = closest_factors[1]

                e_we_decomp = e_we // i_array_value
                e_sn_decomp = e_sn // j_array_value

                # If valid decomposition, print the result and exit
                if e_we_decomp >= smallest_size and e_sn_decomp >= smallest_size:
                    max_procs = i_array_value * j_array_value
                    print("Max # of processors that can be used: ", max_procs)
                    print("Max # of nodes that can be used: 1")
                    break
                else:
                    y -= 1
        else:
            max_procs = i_array_value * j_array_value
            max_nodes = max_procs // cores_orig
            print("Max # of processors that can be used: ", max_procs)
            print("Max # of nodes that can be used: ", max_nodes)
        break

    # Increment cores by the original core count and continue the loop
    x += 1
    cores += cores_orig
