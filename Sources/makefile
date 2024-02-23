#===============================================================================
#
#   SFS Makefile
#
#-------------------------------------------------------------------------------

#-------------------------------------
#   Default options for compilation
#-------------------------------------
FORTRAN ?= nvidia
ASSERT  ?= yes
DEBUG   ?= no
GPU     ?= yes

#--------------------------
#   Variable definitions
#--------------------------

# Directories for objects and modules. (No need to change.)
DIR_BINARY = .
DIR_MODULE = .Modules
DIR_OBJECT = .Objects

# Program name (This should hardly change)
PROGRAM_NAME = No_GPU
ifeq ($(GPU), yes)
  PROGRAM_NAME = With_GPU
endif

PROGRAM_FILE = $(DIR_BINARY)/$(PROGRAM_NAME)

#-------------------------------------------
#   Variables to pass to compiled program
#-------------------------------------------
PASS_ON = -DVFS=1
ifeq ($(ASSERT), yes)
  PASS_ON += -DVFS_ASSERT=1
else
  PASS_ON += -DVFS_ASSERT=0
endif

ifeq ($(DEBUG), yes)
  PASS_ON += -DVFS_DEBUG=1
else
  PASS_ON += -DVFS_DEBUG=0
endif

ifeq ($(GPU), yes)
  PASS_ON += -DVFS_GPU=1
else
  PASS_ON += -DVFS_GPU=0
endif

$(info #=======================================================================)
$(info # Compiling $(PROGRAM_NAME) with compiler $(FORTRAN))
$(info #-----------------------------------------------------------------------)
$(info # Usage:                                                                )
$(info #   make <FORTRAN=nvidia/gnu/intel> <GPU=yes/no> <DEBUG=no/yes>         )
$(info #                                                                       )
$(info # Examples:                                                             )
$(info #   make              - compile with nvidia compiler and GPU support    )
$(info #   make FORTAN=intel - compile with intel compiler                     )
$(info #   make DEBUG=yes    - compile with nvidia compiler in debug mode      )
$(info #-----------------------------------------------------------------------)

#-------------------------------------------------------------------------------
#   Compiler and linker options
#-------------------------------------------------------------------------------
#   Note: Changes only when support to a new Fortran compiler is added.
#-------------------------------------------------------------------------------

#--------------------
#   Fortran == gnu
#--------------------
ifeq ($(FORTRAN), gnu)
  FC = gfortran
  ifeq ($(DEBUG),yes)
    OPT_COMP = -J $(DIR_MODULE) -cpp -fdefault-real-8 -O0 -g  \
                                -Wunused-parameter -Warray-temporaries -Wall
  else
    OPT_COMP = -J $(DIR_MODULE) -cpp -fdefault-real-8 -O3
  endif
  OPT_LINK = $(OPT_COMP)

  $(info # Using GNU Fortran compiler with options: $(OPT_COMP))
endif

#----------------------
#   Fortran == intel
#----------------------
ifeq ($(FORTRAN), intel)
  FC = ifort
  ifeq ($(DEBUG),yes)
    OPT_COMP = -module $(DIR_MODULE) -cpp -r8 -O0 -g -warn all -check all \
               -debug all -fpe-all=0 -traceback
  else
    OPT_COMP = -module $(DIR_MODULE) -cpp -r8 -O3
  endif
  OPT_LINK = $(OPT_COMP)

  $(info # Using Intel Fortran compiler with options: $(OPT_COMP))
endif

#-----------------------
#   Fortran == nvidia
#-----------------------
ifeq ($(FORTRAN), nvidia)
  FC = nvfortran
  ifeq ($(DEBUG),yes)
    OPT_COMP = -module $(DIR_MODULE) -cpp -r8 -O0 -g
  else
    OPT_COMP = -module $(DIR_MODULE) -cpp -r8 -O3
  endif
  ifeq ($(GPU),yes)
    OPT_COMP += -acc -Minfo=accel
  endif
  OPT_LINK = $(OPT_COMP)

  $(info # Using Nvidia Fortran compiler with options: $(OPT_COMP))
endif

#------------------------------------------------------
#   List of sources for modules and functions
#------------------------------------------------------
#   Modules' order must obey their dependency 
#   This list should therefore be written "by hand".
#   Note: Modules written in lower case 
#         letters are candidates for deletion.
#------------------------------------------------------

#-------------
#   Modules
#-------------

# Modules in shared directories
SRC_MOD = Const_Mod.f90		\
          Assert_Mod.f90	\
          Grid_Mod.f90		\
          Matrix_Mod.f90	\
          Linalg_Mod.f90	\
          Native_Mod.f90	\
          Field_Mod.f90		\
          Process_Mod.f90	\
          Gpu_Mod.f90

#---------------
#   Functions
#---------------

# Sources for all functions are obtained by a shell command
SRC_FUN = $(shell ls -1 *.f90			\
                        | xargs -n1 basename	\
                        | grep -v -i _Mod)

#----------------------------------------------------------------------
#   List of objects generated from the list of modules and functions
#----------------------------------------------------------------------
#   Note: This doesn't need editing.
#----------------------------------------------------------------------
OBJ_MOD = $(SRC_MOD:%.f90=$(DIR_OBJECT)/%.o)
OBJ_FUN = $(SRC_FUN:%.f90=$(DIR_OBJECT)/%.o)
OBJ = $(OBJ_MOD) $(OBJ_FUN)

#---------------------------------------------------------
#   Default rule to build Fortran modules and functions
#---------------------------------------------------------
#   Note: This doesn't need editing.
#---------------------------------------------------------

# Modules
$(DIR_OBJECT)/%.o: %.f90 %/*.f90 makefile*
	@echo FC $<
	@$(FC) $(OPT_COMP) $(PASS_ON) -c -o $@ $<

# Functions
$(DIR_OBJECT)/%.o: %.f90 makefile*
	@echo FC $<
	@$(FC) $(OPT_COMP) $(PASS_ON) -c -o $@ $<

#-----------------------------------
#   Rule to build main program
#-----------------------------------
#   Note: Should not be modified.
#-----------------------------------
$(PROGRAM_FILE): $(OBJ)
	@echo Linking "\033[0;32m $(PROGRAM_FILE) \033[0m"
	@$(FC) $(OPT_LINK) -o $(PROGRAM_FILE) $(OBJ)

#--------------------------------------------------------------------
#   Explicit dependencies for modules
#--------------------------------------------------------------------
#   These are automatically generated by:
#   Sources/Utilities/create_external_dependencies_for_makefile.sh
#--------------------------------------------------------------------
include makefile_explicit_dependencies

#---------------------
#   Explicit target.
#---------------------
clean:
	rm -f $(DIR_OBJECT)/*.o $(DIR_MODULE)/*.mod $(PROGRAM_FILE)
