nvfortran -acc -Minfo=accel -cpp -module .Modules/ -c Dense_Mod.f90
nvfortran -acc -Minfo=accel -cpp -module .Modules/ -c Compute_Mod.f90
nvfortran -acc -Minfo=accel -cpp -module .Modules/ -c Main.f90
nvfortran -acc -Minfo=accel -cpp -module .Modules/ Dense_Mod.o Compute_Mod.o Main.o -o With_Gpu

nvfortran                   -cpp -module .Modules/ -c Dense_Mod.f90
nvfortran                   -cpp -module .Modules/ -c Compute_Mod.f90
nvfortran                   -cpp -module .Modules/ -c Main.f90
nvfortran                   -cpp -module .Modules/ Dense_Mod.o Compute_Mod.o Main.o -o No_Gpu

