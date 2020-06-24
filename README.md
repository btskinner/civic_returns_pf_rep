This repository contains the replication files for  

> Skinner, B.T. and Doyle, W.R. (working). Do civic returns to higher
> education differ across subpopulations? An analysis using propensity
> forests

## To run

Clone the project repository, `cd` into project directory, and run the `makefile`:

```bash
git clone https://github.com/btskinner/civic_returns_pf_rep.git
cd ./civic_returns_pf_rep
make
```

Or, after cloning the repository, run the R scripts one by one:

1. `check_packages.r`
1. `get_data.r`  
1. `make_data.r`  
1. `make_descriptives.r`  
1. `fit_trees.r`  
1. `pull_figure_estimates.r`  
1. `make_figures.r`  
1. `make_tables.r`  



