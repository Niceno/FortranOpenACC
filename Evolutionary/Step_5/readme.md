## Introduction: Expansion in Functionality and Software Architecture

This step is a witness to an expansion of functionality as well as advancement in sofwtare architecture.

## New Modules Related to Linear Algebra

In addition to the `Dense_Mod` we had before, and which was a structure holding a dense matrix and the functionality to transfer the dense matrix to and back from _host_ (CPU) and _device_ (GPU), we now have two modules pertinent to linear alebra more: the `Vector_Mod` (holding the `Vector_Type`, a representation of vector in linear algebra) and `Sparse_Mod` (with its `Sparse_Type`), representing the sparse matrix in CSR (compressed sparse row) format.

Each of these modules/classes, contains procedures to transfer from the host to the device (called `Copy_.._To_Device`) and from the device back to host (called `Copy_..._To_Host`).  In addition, each of the linear algebra related classes brings a member function which destroys (deallocates) itself from the device (called `Destroy_..._From_Device`).

## Glimpse of the Things to Come: Computational Grid

This step also introduces Grid\_Type, defined in Grid\_Mod, which defines the connectivity in sparse matrices.  It also hints where these developments are going to.  They are obviously going toward the solution of partial differential equations on numerical grids.

## Extensions to the `Compute_Mod`

`Compute_Mod` has also witnessed a descent share of advancement in this step.  Now it no longer adds two dense matrices, only, it performs a number of tasks with `Dense_Type`, `Sparse_Type` and `Vector_Type`, such as dense-dense matrix addition (like before), dense-dense matrix multiplication, dense-vector multiplication.  As we discussed before, OpenAcc can't operate directly with derived data types, so a workaround in `Compute_Mod` is to write to version of each of the functionality we want: one which takes a operands the derived data types which is just a front-end to the so-colled _accelerated_ variant working only with Fortran basic data types.  The accelerated functions are the one which have extension '_Acc' in their names and it is very important to note that they *do not* check the presence of operands on the device, becuase I learned before that does the copying to and back from the device thus slowing down the GPU version of the code.  So, it is programmer's responsability to ensure that the date he or she needs is on the device before the computation on the device commence.

### New Functionality of the Main Function

Following all these expansions in functionality, the main function changed a lot too.  It doesn't merely call procedures to create dense matrices and add them, it brings up a menu which offers several tests which can be performed at this step and they include:

1 - dense-dense multiplication
2 - dense-vector multiplication
3 - sperse-vector multiplication

I is like a mini-suite of a couple of most basic linear algebra operations on GPUs.

This version has also seen a lot of import of functionality and implementation from other Fortran project, the T-Flows in particular.  Sparse\_Type, Vector\_Type and were coppied from T-Flows and subsequently sized down.  Grid\_Type is only inspired by T-Flows, its functionality is quite different, because it operates only on single-block structured grids.

