################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] make_data.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 14 January 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, "..", args)
dat_dir <- file.path(root, "data")
raw_dir <- file.path(dat_dir,"raw")
cln_dir <- file.path(dat_dir, "clean")
scr_dir <- file.path(root, "scripts")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse", "haven", "labelled"))

## header message
proj_message("make_data.r", "h")

## -----------------------------------------------
## read in codebook for variable selection
## -----------------------------------------------

proj_message("Reading in variable list")
cw <- read_csv(file.path(raw_dir, "varlist.csv"),
               col_types = cols(varname = "c",
                                varlabel = "c",
                                vartype = "c",
                                round = "c",
                                type = "c",
                                restricted = "c",
                                reserve = "i",
                                .default = "i"))

## vector of variables to use
keep_vars <- cw %>%
    filter(keep == 1) %>%
    pull(varname)

## vector of factor variables
factor_vars <- cw %>%
    filter(keep == 1, is_factor == 1) %>%
    pull(varname)

## missing concordance
cw_miss_vars <- cw %>%
    filter(keep == 1, !is.na(reserve)) %>%
    select(varname, reserve)

## vector of variables with missing values to fix
miss_vars <- cw_miss_vars %>% pull(varname)

## -----------------------------------------------
## read in student file
## -----------------------------------------------

## Can download compressed file from:
## https://nces.ed.gov/EDAT/Data/Zip/HSLS_2016_v1_0_Stata_Datasets.zip

## get data and munge
proj_message("Reading in raw HSLS data and munging")
df <- read_dta(file.path(raw_dir, "hsls_16_student_v1_0.dta")) %>%
    ## limit to those in keep vars vector
    select(all_of(keep_vars))

## keep copy of full HSLS for later descriptives
df_full <- df

## -----------------------------------------------
## final munging for main data frame
## -----------------------------------------------

df <- df %>%
    ## filter: must be in base year and must not be missing treatment
    ## Missing:
    ## X4UNIV1 < 10000 (not in base year)
    ## X4EVRATNDCLG < 0
    ## X4PS1START < -7 (-7 represents non-attenders)
    ## Out of sample:
    ## X4PS1START > 201500 (started after volunteer window)
    filter(as.numeric(X4UNIV1) >= 10000,
           X4EVRATNDCLG >= 0,
           (X4PS1START < 201500 & X4PS1START > -8)) %>%
    ## convert negative numbers to proper NA
    val_to_na(miss_vars, cw_miss_vars, varname, reserve) %>%
    ## convert factor variables to actual factors, including NA as level
    mutate_at(vars(factor_vars),
              ~ var_to_factor(.)) %>%
    ## lower names
    rename_all(tolower) %>%
    ## predictors
    mutate(x1stdob = as.numeric(x1stdob)) %>%
    ## collapse Hispanic categories into one (5 --> 4)
    mutate_at(vars(contains("race")),
              ~ ifelse(as.numeric(.) == 5, 4, .)) %>%
    ## convert race/ethnicy vars back to factor
    mutate_at(vars(contains("race")),
              ~ as.factor(.)) %>%
    ## x1 educational expectations: 11 [Don't know] as NA
    mutate_at(vars(matches("x1.+expct")),
              ~ ifelse(. == 11, NA, .)) %>%
    ## x2 educational expectations: 13 [Don't know] as NA
    mutate_at(vars(matches("x2.+expct")),
              ~ ifelse(. == 13, NA, .)) %>%
    ## categories for earnings expectations by degree level
    mutate_at(vars(contains("earn")),
              ~ ordcat_to_rank(.)) %>%
    ## soc codes to bigger aggregate bins
    ## https://www.bls.gov/soc/2018/soc_2018_class_and_coding_structure.pdf
    ## bin    soc
    ## ----------
    ## 1    11-29
    ## 2    31-39
    ## 3    41-43
    ## 4    45-49
    ## 5    51-53
    ## 6       55
    mutate_at(vars(contains("occ2")),
              ~ case_when(. >= 11 & . < 30 ~ 1L,
                          . >= 30 & . < 40 ~ 2L,
                          . >= 40 & . < 44 ~ 3L,
                          . >= 45 & . < 50 ~ 4L,
                          . >= 50 & . < 54 ~ 5L,
                          . == 55 ~ 6L,
                          TRUE ~ NA_integer_) %>%
                  ## to factor
                  var_to_factor(.)) %>%
    ## enrollment
    mutate(t_enrolcol_ever = case_when(
               x4evratndclg == 0 ~ 0L,
               x4evratndclg == 1 ~ 1L)) %>%
    ## outcomes
    mutate(o_vot_reg = case_when(
               s4regvote == 1 ~ 1L,                 # yes
               s4regvote == 2 ~ 0L,                 # no
               s4regvote < 1 ~ NA_integer_,         # non-response
               s4regvote == 3 ~ NA_integer_,        # not eligible
               is.na(s4regvote) ~ NA_integer_),     # missing
           o_vol_ind = case_when(
               s4hrsvoluntr == 0 ~ 0L,              # affirmative 0 hours
               s4hrsvoluntr > 0 ~ 1L,               # affirmative + hours
               s4hrsvoluntr < 0 ~ NA_integer_,      # non-response
               is.na(s4hrsvoluntr) ~ NA_integer_),  # missing
           o_vol_pos = case_when(
               s4hrsvoluntr == 0 ~ NA_integer_,     # affirmative 0 hours
               s4hrsvoluntr > 0 ~ as.integer(s4hrsvoluntr),     # affirmative + hours
               s4hrsvoluntr < 0 ~ NA_integer_,      # non-response
               is.na(s4hrsvoluntr) ~ NA_integer_),  # missing
           o_vol_log = log(o_vol_pos),
           o_vol_hrs = ifelse(s4hrsvoluntr < 0 | is.na(s4hrsvoluntr),
                              NA_integer_,
                              s4hrsvoluntr)) %>%
    select(-starts_with("x4"), -starts_with("s4"))

## -----------------------------------------------
## final munging for comparison data frame
## -----------------------------------------------

df_full <- df_full %>%
    ## convert negative numbers to proper NA
    val_to_na(miss_vars, cw_miss_vars, varname, reserve) %>%
    ## lower names
    rename_all(tolower) %>%
    ## predictors
    mutate(x1stdob = as.numeric(x1stdob)) %>%
    ## collapse Hispanic categories into one (5 --> 4)
    mutate_at(vars(contains("race")),
              ~ ifelse(as.numeric(.) == 5, 4, .)) %>%
    ## convert gender / race/ethnicity to missing if below 0
    mutate(x1sex = ifelse(x1sex < 0, NA, x1sex),
           x1race = ifelse(x1race < 0, NA, x1race)) %>%
    ## enrollment
    mutate(t_enrolcol_ever = case_when(
               x4evratndclg == 0 ~ 0L,
               x4evratndclg == 1 ~ 1L)) %>%
    ## outcomes
    mutate(o_vot_reg = case_when(
               s4regvote == 1 ~ 1L,                 # yes
               s4regvote == 2 ~ 0L,                 # no
               s4regvote < 1 ~ NA_integer_,         # non-response
               s4regvote == 3 ~ NA_integer_,        # not eligible
               is.na(s4regvote) ~ NA_integer_),     # missing
           o_vol_ind = case_when(
               s4hrsvoluntr == 0 ~ 0L,              # affirmative 0 hours
               s4hrsvoluntr > 0 ~ 1L,               # affirmative + hours
               s4hrsvoluntr < 0 ~ NA_integer_,      # non-response
               is.na(s4hrsvoluntr) ~ NA_integer_),  # missing
           o_vol_pos = case_when(
               s4hrsvoluntr == 0 ~ NA_integer_,     # affirmative 0 hours
               s4hrsvoluntr > 0 ~ as.integer(s4hrsvoluntr),     # affirmative + hours
               s4hrsvoluntr < 0 ~ NA_integer_,      # non-response
               is.na(s4hrsvoluntr) ~ NA_integer_),  # missing
           o_vol_log = log(o_vol_pos),
           o_vol_hrs = ifelse(s4hrsvoluntr < 0 | is.na(s4hrsvoluntr),
                              NA_integer_,
                              s4hrsvoluntr)) %>%
    select(-starts_with("x4"), -starts_with("s4"))

## -----------------------------------------------
## save data
## -----------------------------------------------

proj_message("Saving analysis data")
saveRDS(df, file.path(cln_dir, "analysis.rds"))

proj_message("Saving comparison data")
saveRDS(df_full, file.path(cln_dir, "comparison.rds"))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
