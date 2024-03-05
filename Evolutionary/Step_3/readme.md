# Introduction: Extending Functionality a Bit Further

The first evolutionary step (stored in `../Step_001`) showed the basic functionality of Fortran and OpenACC coupling, all within a single programming unit (main function). The second evolutionary step (stored in `../Step_002`) showed that OpenACC commands can spread over different programming units, meaning subroutines in Fortran.

## Motivation: Advancing with Complex Data Types

This example goes a step further and shows how data enclosed inside more complex data types can be transferred to and from GPUs. The program introduces a new type (`Dense_Type`) inside the module (`Dense_Mod`) representing a dense matrix. The `Dense_Type` has only two data members: `n` and `val`.

The `Dense_Type` is still passive; it still uses the main function to transfer the data from the `Dense_Type` to the device and back, using the global functions `Copy_To_Device` and `Copy_From_Device`. Computations are done on the device, calling the global function `Compute_On_Device`.

## Implementation Details: Handling Data Transfer for Derived Types

An important aspect when transferring data belonging to more complex types to and from a "device" is that the derived data types can't be transferred, only its basic data type components, which can be seen in the main function where the transfer takes place.

Another important aspect is that the subroutines performing calculations on the host do not need `!$acc data present ...` and `!$acc end data` clauses if a programmer makes sure the data is on the device.

## Compilation and Execution

The program is compiled with the script `compile.sh` and it requires Nvidia's Fortran compiler, which comes with Nvidia HPC-SDK. The script will create two executables: the `With_Gpu`, which uses GPUs, and `No_Gpu`, which doesn't use GPUs.

