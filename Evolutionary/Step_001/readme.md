# Introduction: Matrix Summation on GPU

In a nutshell, this program computes the sum of three matrices: `c = a + b` on a Graphical Processing Unit (GPU).

# Initial Exploration: Fortran with OpenACC

This was the first attempt (or step) to write a Fortran program which would use OpenACC for GPU acceleration. It is very simple. Three matrices (a, b, and c) are defined on the Central Processing Unit (CPU) (in GPU computing jargon also called the "host"), then they are copied to the GPU (in GPU jargon also called the "device"). Once on the "device", they are summed up (`c = a + b`) in a double loop, and the result is copied back to the "host" from where they are printed.

# Core Principles: From Simple to Complex CFD Simulations

Although ludicrously simple, this little program contains all necessary ingredients a typical Computational Fluid Dynamics (CFD) simulation would have:

1. Creation of data on the CPU ("host")
2. Copying of data to the GPU ("device")
3. Performing a calculation on the GPU ("device")
4. Copying the data back to the "host", and
5. Save data for post-processing (just printing in this simple case.)

# Technical Deep Dive: Data Management and Computation

Please note the difference in which operand matrices (a and b) and the result matrix (c) are passed to "device". Matrices a and b are copied with the command `!$acc data copyin(a, b)`, because they are not copied back to the "host" after the calculations. Contrary to that, matrix "c" will hold the result of the operation on the "device" and we want its contents back once the calculation is over. Therefore, we copy it with the command `!$acc data copy(c)!`, which means that data will be copied back once the command `!$acc end data` is reached.

Although this works, we could have done things slightly better here. The initial contents of matrix c are over-written during the calculation, so we could have just reserved a memory for matrix c on the "device" with the command: `!$acc enter data create(c)`. This command would only allocate memory for matrix c on GPU, without copying the contents. In such a case data would have to be transferred back to the "host" with command: `!$acc exit data copyout(c)`.

The calculations on the device are performed using the Keep It Simple and Stupid (KISS) principle, that is, they are started with `!acc kernels` and ended with `!$acc end kernels`. That gave a decent speed up of this case. Not much philosophy, just tell the compiler to run on the "device" in the best way it can.

# Building and Running: Compilation Details

The program is compiled with the script `compile.sh` and it requires Nvidia's Fortran compiler, which comes with Nvidia HPC-SDK. The script will create two executables: the `With_Gpu` which uses GPUs and `No_Gpu` which doesn't use GPUs.

