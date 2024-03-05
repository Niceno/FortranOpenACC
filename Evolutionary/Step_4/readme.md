## Further Evolution: Enhancing Data Types and Modularization

### Enhanced Data Handling with Member Functions

In the previous example (`../Step_3`), the data for our calculations were members of a derived data type `Dense_Type`. The data was transferred from the _host_ to the _device_ with global functions `Copy_To_Device`, `Create_On_Device`, etc. Computations too were done by a global function `Compute_On_Device`.

Here we make a step further and give the new data type (`Dense_Type`), the ability to transfer its data members to the device with its own member functions (`Copy_To_Device`, `Create_On_Device`, ..., `Copy_From_Device`).

### Rethinking Data Transfer Philosophy

The philosophy here was that it was most convenient to write the data transfer functions close to the data itself. Although that is undeniably true, it might not be the most economical way to transfer data to the device because we might not need all data members on device, and such a strategy was eventually abandoned in the bigger project in the directory `Sources`.

### Modularization and Calculation Enhancements

Another change to the previous step is that calculations are done in a different module, called `Compute_Mod`, which introduces the new type `Compute_Type`, and which also creates a singleton object called `Global`, indicating that it is accessible to the rest of the code.  It is important to note that the `Compute_Mod` is unaware of the derived data type `Dense\_Type`, and accepts as operators basic Fortran's two-dimensional arrays.  The reason for that was discussed elsewhere: OpenACC is not aware of derived data types in a program.

This step also brings several source files to the project. We have the main function still, but each module is defined in its own source file for the module definition, and sources in its sub-directories for member functions (the same practice was introduced in `T-Flows` many years back, and it is still proving to be useful).

### Compilation and Execution

The program is compiled with the script `compile.sh` and it requires Nvidia's Fortran compiler, which comes with Nvidia HPC-SDK. The script will create two executables: the `With_Gpu` which uses GPUs and `No_Gpu` which doesn't use GPUs.

