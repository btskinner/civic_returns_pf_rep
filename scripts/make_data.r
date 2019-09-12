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
root <- ifelse(length(args) == 0, '..', args)
dat_dir <- file.path(root, 'data')
raw_dir <- file.path(dat_dir,'raw')
cln_dir <- file.path(dat_dir, 'clean')
scr_dir <- file.path(root, 'scripts')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','haven','Tmisc'))

## header message
proj_message('make_data.r','h')

## -----------------------------------------------
## read in codebook for variable selection
## -----------------------------------------------

proj_message('Reading in variable list')
cw <- read_csv(file.path(raw_dir, 'varlist.csv'),
               col_types = cols(varname = 'c',
                                varlabel = 'c',
                                vartype = 'c',
                                round = 'c',
                                type = 'c',
                                restricted = 'c',
                                reserve = 'i',
                                .default = 'i'))

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
    filter(!is.na(reserve)) %>%
    select(varname, reserve)

## vector of variables with missing values to fix
miss_vars <- cw_miss_vars %>% pull(varname)

## check vars
check_vars <- c('W1STUDENT', setdiff(miss_vars, 'X1STDOB'))

## -----------------------------------------------
## read in student file
## -----------------------------------------------

## Can download compressed file from:
## https://nces.ed.gov/EDAT/Data/Zip/HSLS_2016_v1_0_Stata_Datasets.zip

## get data and munge
proj_message('Reading in raw HSLS data and munging')
df <- read_dta(file.path(raw_dir, 'hsls_16_student_v1_0.dta')) %>%
    ## filter: must be in base year and must not be missing treatment
    ## Missing:
    ## X4UNIV1 < 10000 (not in base year): 2059
    ## S3CLASSES < 0: 5004
    filter(as.numeric(X4UNIV1) >= 10000,
           as.numeric(S3CLASSES) > 0) %>%
    ## limit to those in keep vars vector
    select(one_of(keep_vars))

## get small data frame to check for consistency later due to bind_cols join
df_sm_1 <- select(df, one_of(check_vars)) %>% zap_labels

## -----------------------------------------------
## fix missing
## -----------------------------------------------

## fix missing dynamically (everything stays aligned so can just cbind later)
df_miss_fix <- map(miss_vars, ~ val_to_na(df, .x, cw_miss_vars)) %>%
    bind_cols %>%
    setNames(miss_vars)

## drop missing columns from main dataset and replace with fixed
df <- df %>%
    select(-one_of(miss_vars)) %>%
    ## can bind_cols() since no ordering changed, just selective replacement,
    bind_cols(df_miss_fix)

## get small data frame to check for consistency later due to bind_cols join
df_sm_2 <- select(df, one_of(check_vars))

## -----------------------------------------------
## check join
## -----------------------------------------------

## B/c we're not joining on key, but rather just binding columns,
## confirm that everything aligns as it should. The student weighting
## variable (w1student) isn't missing values, so it will be the
## implicit key. We compare the df_sm_* data frames we saved above,
## which contain 'w1student' as well as the variables for which we
## converted negative to <NA>. The test is to:
##
## (1) get index of rows with NAs from second df
## (2) subset both first and second dfs with this index
## (3) test for equality
##
## The two subset data frames should be equal if everything is aligned
## as it should be.

## get index of missing rows from second df
index <- df_sm_2 %>%
    rownames_to_column('index') %>%
    drop_na %>%
    pull(index) %>%
    as.integer

## confirm if equal, stop if not
if (!all_equal(df_sm_1[index,], df_sm_2[index,])) {
    stop('Poor join after missing fix; check code and data')
}

## -----------------------------------------------
## final munging
## -----------------------------------------------

df <- df %>%
    ## convert factor variables to actual factors, including NA as level
    mutate_at(vars(factor_vars),
              ~var_to_factor(.)) %>%
    ## lower names
    rename_all(tolower) %>%
    ## predictors
    mutate(x1stdob = as.numeric(x1stdob)) %>%
    ## enrollment ("don't know" [3] is joined with "no" [2])
    mutate(enrolcol_201311 = case_when(
               s3classes == 1 ~ 1L,
               s3classes > 1 ~ 0L)) %>%
    ## outcomes
    mutate(vot_reg = case_when(
               s4regvote == 1 ~ 1L,
               s4regvote == 2 ~ 0L,
               s4regvote < 1 ~ NA_integer_,
               s4regvote == 3 ~ NA_integer_,
               is.na(s4regvote) ~ NA_integer_),
           vol_ind = case_when(
               s4hrsvoluntr == 0 ~ 0L,
               s4hrsvoluntr > 0 ~ 1L,
               s4hrsvoluntr < 0 ~ NA_integer_,
               is.na(s4hrsvoluntr) ~ NA_integer_),
           vol_hrs = ifelse(s4hrsvoluntr < 0 | is.na(s4hrsvoluntr),
                            NA_integer_,
                            s4hrsvoluntr),
           job_bal = case_when(
               s4jobbalance == 1 ~ 1L,
               s4jobbalance == 2 ~ 0L,
               s4jobbalance == 3 ~ -1L,
               is.na(s4jobbalance) ~ NA_integer_),
           job_con = case_when(
               s4jobcontrib == 1 ~ 1L,
               s4jobcontrib == 2 ~ 0L,
               s4jobcontrib == 3 ~ -1L,
               is.na(s4jobcontrib) ~ NA_integer_),
           ## flags for missing outcomes
           vot_reg_m = ifelse(is.na(vot_reg), 1, 0),
           vol_ind_m = ifelse(is.na(vol_ind), 1, 0),
           vol_hrs_m = ifelse(is.na(vol_hrs), 1, 0),
           job_bal_m = ifelse(is.na(job_bal), 1, 0),
           job_con_m = ifelse(is.na(job_con), 1, 0))

## -----------------------------------------------
## check missingness by outcome
## -----------------------------------------------

proj_message('Creating missing data frame')

## We'll use this data to decide which variables to include in each
## CF. For variables with missingness > X, we won't use them as
## predictors. For those < X, we'll use casewise deletion of
## observations.

## get outcomes to walk through
outcomes <- c('vot_reg','vol_hrs','vol_ind','job_bal','job_con')

## also drop other variables that we don't use as predictors
drop_x <- c(grep('^x4|^s3|^s4|^w|_m$', names(df), value = TRUE), outcomes)

## get tibble of missing values by outcome
## NB: b/c we make factors consider NA a level, they aren't missing
## here, only continuous variables are
df_miss <- map(outcomes,
               ~ df %>%
                   filter(!!sym(paste0(.x, '_m')) == 0) %>%
                   select(-one_of(outcomes, drop_x)) %>%
                   Tmisc::propmiss(.) %>%
                   mutate(outcome_v = .x)) %>%
    bind_rows

saveRDS(df_miss, file.path(cln_dir, 'missing.rds'))

## -----------------------------------------------
## save data
## -----------------------------------------------

proj_message('Saving analysis data')
saveRDS(df, file.path(cln_dir, 'analysis.rds'))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
rm(df_miss)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
