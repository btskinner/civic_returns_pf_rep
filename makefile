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
EST_DIR := estimates
FIG_DIR := figures
SCR_DIR := scripts
TAB_DIR := tables

# --- variables ----------------------------------

# data vars
nces := https://nces.ed.gov/
hsls_zip := $(DAT_DIR)/raw/HSLS_2016_v1_0_CSV_Datasets.zip
hsls_fil := $(DAT_DIR)/raw/hsls_16_student_v1_0.dta
hsls_url := $(nces)/EDAT/Data/Zip/HSLS_2016_v1_0_Stata_Datasets.zip
analysis_dat := $(DAT_DIR)/clean/analysis.rds

# output vars (one example: assumes one change is all change)
des_output := $(DES_DIR)/descriptives.rds
est_output := $(EST_DIR)/vot_reg_cf.rds
ate_output := $(EST_DIR)/vot_reg_cf_est.csv
fig_output := $(FIG_DIR)/vv_gender_race.pdf
tab_output := $(TAB_DIR)/varlist.tex

# --- build targets ------------------------------

all: setup data descriptives analysis figures tables paper

data_get: $(hsls_zip) $(hsls_fil)
data_clean: $(analysis_dat)
data: data_get data_clean
descriptives: $(des_output)
analysis: $(est_output)
figures: $(fig_output)
tables: $(tab_output)

.PHONY: all analysis clean data data_get data_clean descriptives figures tables setup 

# --- data ---------------------------------------

# get zip
$(hsls_zip):
ifeq ($(wildcard $(hsls_zip)),)
	@echo "Downloading raw data files"
	curl -o $@ $(hsls_url)
endif

# unzip
$(hsls_fil): $(hsls_zip)
ifeq ($(wildcard $(hsls_fil)),)
	@echo "Unzipping raw data files"
	unzip $< -d $(DAT_DIR)/raw
endif

# --- packages -----------------------------------

setup: $(SCR_DIR)/check_packages.r
	@echo "Checking for and installing necessary R packages"
	Rscript $< .

# --- clean data ---------------------------------

$(analysis_dat): $(SCR_DIR)/make_data.r $(SCR_DIR)/utils.r
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

# --- clean up -----------------------------------

clean:
	@echo "Cleaning up directory"
	$(RM) -r $(EST_DIR)/* $(DES_DIR)/* $(DAT_DIR)/clean/* $(TAB_DIR)/*
	$(RM) -r $(FIG_DIR)/*

# ------------------------------------------------------------------------------
# end makefile
# ==============================================================================
