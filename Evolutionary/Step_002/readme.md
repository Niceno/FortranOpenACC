# Extending Functionality

In a nutshell, this program serves the same function as the previous step (stored in `../Step_001`), which is to compute the sum of three matrices: `c = a + b` on a GPU.

## Motivation: Beyond the Basics

Although the first step to write a Fortran program and couple it with GPU functionality was a success, and although that first step entailed all the functionality a typical CFD simulation on GPUs would entail (1 - creation of data on the "host"; 2 - copying the data to the "device"; 3- performing the computations on the "device"; 4 - copying the computed data back to "host" and 5 - post-processing the results on the "host"), many questions remained open.

## Expanding the Framework: Modularization Challenges

Given that the first step had all the functionality within one source file spanning a mere couple of dozens of lines, the question that arises next is how to embed this functionality within a bigger project? How to deal with Fortran-GPU coupling inside a program that has many modules, a program whose data is passed from one module to another, a program that spans several hundreds or even thousands of lines of code?

## Implementation: Splitting the Task

What this step brings is the main function which creates the data on the "host" and makes the post-processing (still only printing), but copying to "device", computations on "device", and copying the data back to the "host" are performed by different global functions here.

## Compilation and Execution

The program is compiled with the script `compile.sh` and it requires Nvidia's Fortran compiler, which comes with Nvidia HPC-SDK. The script will create two executables: the `With_Gpu` which uses GPUs and `No_Gpu` which doesn't use GPUs.

