nvfortran                   Main.f90 -o No_Gpu
nvfortran -acc -Minfo=accel Main.f90 -o With_Gpu
