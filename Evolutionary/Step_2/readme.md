## Extending Functionality

In a nutshell, this program serves the same function as the previous step (stored in `../Step_1`), which is to compute the sum of three matrices: `c = a + b` on a GPU.

### Motivation: Beyond the Basics

Although ,y first step to write a Fortran program and couple it with GPU functionality was a success, and although that first step entailed all the functionality a typical CFD simulation on GPUs would entail (1 - creation of data on the CPU (_host_); 2 - copying the data to the GPU (_device_); 3- performing the computations on the device; 4 - copying the computed data back to host and 5 - post-processing the results on the host), many questions remained open in my mind.

### Expanding the Framework: Modularization Challenges

Given that the first step had all the functionality within one source file spanning a mere couple of dozens of lines, the question that arose in my mind is how to embed this functionality within a bigger project? How to deal with Fortran-GPU coupling inside a program that has many modules which define derived data types and member functions, a program whose data is passed from one module to another, a program that spans over several hundreds or even thousands of lines of code?

### Implementation: Splitting the Task

What this step brings is the main function which creates the data on the host and makes the post-processing (still only printing), but copying to the device, computations on the device, and copying the data back to the host are performed by different global functions (called `Copy\_To\_Device`, `Create\_On\_Device`, `Copy\_From\_Device` and `Compute\_On\_Device` in this step.  All the functions are defined inside one file, the `Main.f90`.

### Compilation and Execution

The program is compiled with the script `compile.sh` and it requires Nvidia's Fortran compiler, which comes with Nvidia HPC-SDK. The script will create two executables: the `With_Gpu` which uses GPUs and `No_Gpu` which doesn't use GPUs.

