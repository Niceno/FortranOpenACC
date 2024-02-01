nvfortran                   Example_001.f90 -o No_Gpu
nvfortran -acc -Minfo=accel Example_001.f90 -o With_Gpu
