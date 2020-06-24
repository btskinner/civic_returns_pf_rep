################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] analysis.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 14 January 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, '..', args)
data_dir <- file.path(root, 'data')
cln_dir <- file.path(data_dir, 'clean')
est_dir <- file.path(root, 'estimates')
scr_dir <- file.path(root, 'scripts')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','grf','caret'))

## header message
proj_message('fit_trees.r','h')

## -----------------------------------------------
## options
## -----------------------------------------------

num_threads <- parallel::detectCores()
num_trees <- 20000

## -----------------------------------------------
## utility function
## -----------------------------------------------

run_cf <- function(df,
                   df_miss,
                   outcome,
                   treatment,
                   drop_x = NULL,
                   weights = NULL,
                   missing_upper_limit = 0.03,
                   ...) {
    ## get missingness indicator for outcome
    outcome_m <- paste(outcome, 'm', sep = '_')
    ## subset data based on missingness in outcome (0 := not missing)
    df <- filter(df, .data[[outcome_m]] == 0)
    ## filter to missing information for this outcome
    df_miss <- filter(df_miss, outcome_v == outcome)
    ## removing rhs vars if missingness is > defined upper limit
    drop_vars <- filter(df_miss, propmiss > missing_upper_limit) %>% pull(var)
    df <- select(df, -one_of(drop_vars))
    ## complete case analysis for remaining (drop observation if any NAs)
    df <- drop_na(df, -c(outcome, treatment, drop_x, weights))
    ## outcome
    y <- df %>% select(.data[[outcome]]) %>% pull
    ## treatment
    w <- df %>% select(.data[[treatment]]) %>% pull
    ## covariates
    rhs <- select(df, -one_of(outcome, treatment, drop_x, weights))
    rhs_fac <- select_if(rhs, is.factor) # factor
    rhs_num <- select_if(rhs, is.numeric) # numeric
    ## dummy out factors (complete, not full rank [i.e., both 0 & 1 included])
    x <- caret::dummyVars(~ ., rhs_fac) %>% predict(., newdata = rhs_fac)
    ## bind dummy matrix back to numeric columns
    x <- cbind(x, rhs_num)
    ## if weights...
    if (!is.null(weights)) {
        ## sample weights
        sw <- df %>% select(.data[[weights]]) %>% pull
        ## message
        proj_message(paste('Running weighted propensity forest for: ',
                           outcome))
        ## run forest
        cf <- causal_forest(X = x, Y = y, W = w, sample.weights = sw, ...)
    } else {
        ## message
        proj_message(paste('Running unweighted propensity forest for: ',
                           outcome))
        ## run forest
        cf <- causal_forest(X = x, Y = y, W = w, ...)
    }
    ## return
    return(cf)
}

run_cf_vi <- function(cf_path, keep_num = 10) {
    ## get causal forest object basename
    cf_name <- get_basename(cf_path)
    ## read in causal forest object
    cf <- readRDS(cf_path)
    ## pull column names
    x_names <- colnames(cf$X.orig)
    ## always keep certain covariates
    keep_names <- grep('x1sex|x1race|x1poverty185', x_names, value = TRUE)
    ## get variable importance
    vi <- variable_importance(cf)
    ## set the names to keep: alway + most important #
    sort_order <- order(vi, decreasing = TRUE)
    keep_names <- c(keep_names,
                    setdiff(colnames(cf$X.orig)[sort_order],
                            keep_names)[1:keep_num])
    ## if weights...
    if (any(grepl('sample.weights', names(cf)))) {
        ## message
        proj_message(paste0('Re-running weighted propensity forest ',
                            'with most important variables for: ',
                            cf_name))
        ## run new forest
        ncf <- with(cf,
                    causal_forest(X = X.orig[,keep_names],
                                  Y = Y.orig,
                                  W = W.orig,
                                  sample.weights = sample.weights))
    } else {
        ## message
        proj_message(paste0('Re-running unweighted propensity forest ',
                            'with most important variables for: ',
                            cf_name))
        ## run new forest
        ncf <- with(cf,
                    causal_forest(X = X.orig[,keep_names],
                                  Y = Y.orig,
                                  W = W.orig))
    }
    ## return
    return(ncf)
}

## -----------------------------------------------
## read in data
## -----------------------------------------------

proj_message('Reading in analysis data')
df <- readRDS(file.path(cln_dir, 'analysis.rds'))
df_miss <- readRDS(file.path(cln_dir, 'missing.rds'))

## -----------------------------------------------
## propensity forest
## -----------------------------------------------

proj_message('Setting up outcomes, treatment, drop vars, and weights')

## outcomes
outcomes <- c('vot_reg','vol_hrs','vol_ind','job_bal','job_con')

## treatment
treatment <- grep('enrolcol', names(df), value = TRUE)

## drop student id, outcomes, and all weights/variables from 2nd follow-up
drop_x <- c(grep('^x4|^s3|^s4|^w|_m$', names(df), value = TRUE), outcomes)

## sample weights
weights <- grep('^w1', names(df), value = TRUE)

## -----------------------------------------------
## run/write primary to disk
## -----------------------------------------------

## note about number of threads and trees
proj_message(paste('NOTE: Using', num_threads, 'threads to fit', num_trees,
                   'trees for each forest'))

## unweighted
walk(outcomes,
     ~ run_cf(df,
              df_miss,
              outcome = .x,
              treatment = treatment,
              drop_x = drop_x,
              num.threads = num_threads,
              num.trees = num_trees) %>%
         saveRDS(file.path(est_dir,
                           paste0(.x, '_cf.rds'))))

## weighted
walk(outcomes,
     ~ run_cf(df,
              df_miss,
              outcome = .x,
              treatment = treatment,
              drop_x = drop_x,
              weights = weights,
              num.threads = num_threads,
              num.trees = num_trees) %>%
         saveRDS(file.path(est_dir,
                           paste0(.x, '_cfw.rds'))))

## -----------------------------------------------
## run/write most important to disk
## -----------------------------------------------

## read in files just created
files <- list.files(file.path(est_dir), '*.rds', full.names = TRUE)

## unweighted
files_uw <- grep('_cfw', files, value = TRUE, invert = TRUE)
walk(files_uw,
     ~ run_cf_vi(.x) %>%
         saveRDS(file.path(est_dir,
                           paste0(get_basename(.x), '_vi.rds'))))

## weighted
files_w <- setdiff(files, files_uw)
walk(files_w,
     ~ run_cf_vi(.x) %>%
         saveRDS(file.path(est_dir,
                           paste0(get_basename(.x), '_vi.rds'))))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
