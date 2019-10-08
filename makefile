# ==============================================================================
# 
# [ PROJ ] Postsecondary civic outcomes
# [ FILE ] makefile
# [ AUTH ] Benjamin Skinner & Will Doyle
# [ INIT ] 14 January 2019
#
# ==============================================================================

# --- directories --------------------------------

DAT_DIR := data
DES_DIR := descriptives
DOC_DIR := docs
EST_DIR := estimates
FIG_DIR := figures
SCR_DIR := scripts
TAB_DIR := tables

# --- variables ----------------------------------

# data vars
analysis_dat := $(DAT_DIR)/clean/analysis.rds

# output vars (one example: assumes one change is all change)
des_output := $(DES_DIR)/descriptives.rds
est_output := $(EST_DIR)/vot_reg_cf.rds
ate_output := $(EST_DIR)/vot_reg_cf_est.csv
fig_output := $(FIG_DIR)/vv_gender_race.pdf
tab_output := $(TAB_DIR)/predlab.tex
doc_output := $(DOC_DIR)/paper_tabfig.tex

# --- build targets ------------------------------

all: setup data descriptives analysis figures tables doc

descriptives: $(des_output)
docs: $(doc_output)
analysis: $(est_output)
figures: $(fig_output)
tables: $(tab_output)

.PHONY: all analysis clean data descriptives doc figures tables setup 

# --- packages -----------------------------------

setup: $(SCR_DIR)/check_packages.r
	@echo "Checking for and installing necessary R packages"
	Rscript $< .

# --- get data -----------------------------------

data: $(SCR_DIR)/get_data.r
	@echo "Checking for and, if necessary, downloading NCES data"
	@mkdir -p $(DAT_DIR)/raw
	Rscript $< .

# --- clean data ---------------------------------

$(analysis_dat): $(SCR_DIR)/make_data.r $(SCR_DIR)/utils.r $(hsls_fil)
	@echo "Cleaning data"
	@mkdir -p $(DAT_DIR)/clean
	Rscript $< .

# --- descriptives -------------------------------

$(des_output): $(SCR_DIR)/make_descriptives.r $(analysis_dat) 
	@echo "Getting descriptives"
	@mkdir -p $(DES_DIR)
	Rscript $< .

# --- analysis -----------------------------------

$(ate_output): $(SCR_DIR)/pull_figure_estimates.r $(est_output) 
	@echo "Fitting trees and storing objects (takes a while)"
	Rscript $< .

$(est_output): $(SCR_DIR)/fit_trees.r $(analysis_dat) 
	@echo "Fitting trees and storing objects (takes a while)"
	@mkdir -p $(EST_DIR)
	Rscript $< .

# --- tables & figures ---------------------------

$(tab_output): $(SCR_DIR)/make_tables.r $(ate_output)
	@echo "Making tables"
	@mkdir -p $(TAB_DIR)	
	Rscript $< .

$(fig_output): $(SCR_DIR)/make_figures.r $(ate_output) $(des_output)
	@echo "Making figures"
	@mkdir -p $(FIG_DIR)
	Rscript $< .

# --- documents ----------------------------------

$(doc_output): $(tab_output) $(fig_output)
	@echo "Compiling documents"	
	pdflatex $@

# --- clean up -----------------------------------

clean:
	@echo "Cleaning up directory"
	$(RM) -r $(EST_DIR)/* $(DES_DIR)/* $(DAT_DIR)/clean/* $(DAT_DIR)/raw/*
	$(RM) -r $(TAB_DIR)/* $(FIG_DIR)/*

# ------------------------------------------------------------------------------
# end makefile
# ==============================================================================
