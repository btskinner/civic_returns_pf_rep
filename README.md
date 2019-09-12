This repository contains the replication files for  

> Skinner, B.T. and Doyle, W.R. (working). Are civic returns to higher education different across
  subpopulations? An analysis using propensity forests

## To run

Clone the project repository, `cd` into project directory, and run the `makefile`:

```bash
git clone https://github.com/btskinner/civic_returns_pf.git
cd ./civic_returns_pf
make
```

Or, after cloning the repository, run the R scripts one by one:

1. `check_packages.r`  
1. `make_data.r`  
1. `make_descriptives.r`  
1. `fit_trees.r`  
1. `pull_figure_estimates.r`  
1. `make_figures.r`  
1. `make_tables.r`  

## Download HSLS raw data file

If not using the `makefile`, you need to download
`hsls_16_student_v1_0.csv` and place in `data/raw` subdirectory.


