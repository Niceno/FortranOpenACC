## Introduction: Further Expansions and Consilidations

Previous step (`../Step_5`) already gave contours of something more serious than
a mere code snippet: it introduced a makefile, a number of modules (practically
objects) with well defined and rounded functionality.  It also introduced the
`Grid_Mod`, a module which defines a computational grid, defining the sparsity
pattern for sparse matrices.  In this step, I continued in the same direction,
but also conclued this step-wise approach, as it wasn't any longer as efficient
as a full-fledged software development under the Github's version control
system.

### Outline of Novelties in This Step

What was `Compute_Mod` in the previous steps, is now called `Linalg_Mod`,
clearly showing that its main purpose is to embed the linear algebra
functionality.  The number of basic functions grew to seven which, together
with their _accelerated_ counterpart makes a total of fourteen member functions
and they include operations like on dense matrices, sparse matrices, matrix
vector products, vector dot products as well as vector operations of the form:
`c = a + s * b`, where `s` is a scalar and `a`, `b` and `c` are vectors.

The expansion of functionality of the `Linalg_Mod`, led to bigger number of
tests which main function offer, totalling five now:

1. dense-matrix dense-matrix product
2. dense-matrix vector product
3. sparse-matrix vector product
4. vector vector dot product
5. operations: `c = a + scalar * b` and `c = a - scalar * b`

Each test comes with its own source file, called `Test_001.f90` ... `Test_005.f90`.

### The End of the Line for the Step-Wise Approach

Given the overall structure of the program in this step, and given that the
`Linalg_Mod` has, in essence, all functionality to be used as building blocks
of iterative linear system solvers, and given that we already have a
computational grid defined in `Grid_Mod`, furhter steps would have to go
towards implementation of discretization method, both for conservation
equations and gradients, as well as other numerical algorithms not strictly
related to linear algebra alone.  Given the diversity in which the program
will develop in the future, this step-wise approach is better to be left
behind and continue with a full-fluedged software development under the
controlf of Github.  This development continues in the sub-directory
`../Sources`


