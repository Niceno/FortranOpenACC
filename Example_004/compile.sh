nvfortran                   -cpp Example_004.f90 -o No_Gpu
nvfortran -acc -Minfo=accel -cpp Example_004.f90 -o With_Gpu
