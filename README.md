# Civic returns to college: HSLS + propensity forests

## To run

`cd` into project directory and run the `makefile`:

```shell
make
```

Or do it piece by piece:

1. `check_packages.r`  
1. `make_data.r`  
1. `make_descriptives.r`  
1. `fit_trees.r`  
1. `pull_figure_estimates.r`  
1. `make_figures.r`  
1. `make_tables.r`  

## Download HSLS raw data file

If not using the makefile, you need to download
`hsls_16_student_v1_0.csv` and place in `data/raw` subdirectory.

