#
# Examples can be found in the directories:
#
# - example_mixed_type1
# - example_mixed_type2
# - example_prism_type1
# - example_prism_type2
# - example_tetra_type1
# - example_tetra_type2
#

# For example, go into the directory "example_prism_type2"

cd example_prism_type2

# Then look at readme.txt in there, which has the following commands:

##########################################
# Compile the code

gfortran -O2 ../sphere_grid_v06.f90

# Run

./a.out


# Generate figures (if you have Tecplot)

tec360 -b generate_figs.mcr
##########################################

