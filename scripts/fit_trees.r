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
root <- ifelse(length(args) == 0, "..", args)
data_dir <- file.path(root, "data")
cln_dir <- file.path(data_dir, "clean")
est_dir <- file.path(root, "estimates")
scr_dir <- file.path(root, "scripts")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse","grf","caret","Tmisc"))

## header message
proj_message("fit_trees.r","h")

## -----------------------------------------------
## options
## -----------------------------------------------

num_threads <- parallel::detectCores()
num_trees <- 20000

## -----------------------------------------------
## utility function
## -----------------------------------------------

run_cf <- function(df,
                   outcome,
                   treatment,
                   drop_x = NULL,
                   weights = NULL,
                   missing_upper_limit = 0.50,
                   ...) {
    ## drop observations with 0 weights if using weights
    if (!is.null(weights)) {
        df <- filter(df, !!sym(weights) != 0)
    }
    ## subset data based on missingness in outcome (0 := not missing)
    df <- filter(df, !is.na(!!sym(outcome)))
    ## get missingness based on current sample
    df_miss <- df %>%
        select(-all_of(drop_x)) %>%
        Tmisc::propmiss(.)
    ## removing rhs vars if missingness is > defined upper limit
    drop_vars <- filter(df_miss, propmiss > missing_upper_limit) %>% pull(var)
    df <- select(df, -all_of(drop_vars))
    ## outcome
    y <- df %>% pull(!!sym(outcome))
    ## treatment
    w <- df %>% pull(!!sym(treatment))
    ## covariates
    rhs <- select(df, -any_of(drop_x))
    rhs_fac <- select_if(rhs, is.factor)  # factor
    rhs_num <- select_if(rhs, is.numeric) # numeric
    ## dummy out factors (complete, not full rank [i.e., both 0 & 1 included])
    x <- caret::dummyVars(~ ., rhs_fac) %>% predict(., newdata = rhs_fac)
    ## bind dummy matrix back to numeric columns
    x <- cbind(x, rhs_num)
    ## if weights...
    if (!is.null(weights)) {
        ## sample weights
        sw <- df %>% pull(!!sym(weights))
        ## message
        proj_message(paste("Running weighted propensity forest for: ",
                           outcome))
        ## run forest
        cf <- causal_forest(X = x, Y = y, W = w, sample.weights = sw, ...)
    } else {
        ## message
        proj_message(paste("Running unweighted propensity forest for: ",
                           outcome))
        ## run forest
        cf <- causal_forest(X = x, Y = y, W = w, ...)
    }
    ## return
    return(cf)
}

run_cf_vi <- function(cf_path, keep_pos = FALSE, keep_quant = .9) {
    ## get causal forest object basename
    cf_name <- get_basename(cf_path)
    ## read in causal forest object
    cf <- readRDS(cf_path)
    ## get variable importance
    vi <- variable_importance(cf)
    ## pull column names
    x_names <- colnames(cf$X.orig)
    ## put into tibble
    df_vi <- tibble(var = x_names, vimp = c(vi)) %>% arrange(desc(vimp))
    ## always keep certain covariates
    keep_names <- grep("x1sex|x1race|x1poverty185", x_names, value = TRUE)
    ## set the names to keep: always + most important #
    if (keep_pos) {
        vi_names <- filter(df_vi, vimp > 0) %>% pull(var)
    } else {
        vi_names <- filter(df_vi, vimp >= quantile(vimp, keep_quant)) %>% pull(var)
    }
    ## combine keep_names with vi_names (only unique, no need to double)
    keep_names <- c(keep_names, vi_names) %>% unique
    ## message
    vi_type <- case_when(
        isTRUE(keep_pos) ~ "(non-zero) ",
        TRUE ~ paste0("(Pr >= ", keep_quant, ") ")
    )

    if (any(grepl("sample.weights", names(cf)))) {
        ## message
        proj_message(paste0("Re-running weighted propensity forest ",
                            "with most important ",
                            vi_type,
                            "variables for: ",
                            cf_name))
        ## run new forest
        ncf <- with(cf,
                    causal_forest(X = X.orig[,keep_names],
                                  Y = Y.orig,
                                  W = W.orig,
                                  sample.weights = sample.weights))
    } else {
        ## message
        proj_message(paste0("Re-running unweighted propensity forest ",
                            "with most important ",
                            vi_type,
                            "variables for: ",
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

proj_message("Reading in analysis data")
df <- readRDS(file.path(cln_dir, "analysis.rds"))

## -----------------------------------------------
## propensity forest
## -----------------------------------------------

proj_message("Setting up outcomes, treatment, drop vars, and weights")

## outcomes
outcomes <- names(df) %>% str_subset("^o_")

## treatment
treatment <- names(df) %>% str_subset("^t_")

## sample weights
weights <- names(df) %>% str_subset("^w")

## drop student id, outcomes, and all weights/variables from 2nd follow-up
drop_x <- c(outcomes, treatment, weights)

## -----------------------------------------------
## run/write primary to disk
## -----------------------------------------------

## note about number of threads and trees
proj_message(paste("NOTE: Using", num_threads, "threads to fit", num_trees,
                   "trees for each forest"))

## unweighted
walk(outcomes,
     ~ run_cf(df,
              outcome = .x,
              treatment = treatment,
              drop_x = drop_x,
              num.threads = num_threads,
              num.trees = num_trees) %>%
         saveRDS(file.path(est_dir,
                           paste0(.x, "_cf.rds"))))

## weighted
walk(outcomes,
     ~ run_cf(df,
              outcome = .x,
              treatment = treatment,
              drop_x = drop_x,
              weights = weights,
              num.threads = num_threads,
              num.trees = num_trees) %>%
         saveRDS(file.path(est_dir,
                           paste0(.x, "_cfw.rds"))))

## -----------------------------------------------
## run/write most important to disk
## -----------------------------------------------

## read in files just created
files <- list.files(file.path(est_dir), "*.rds", full.names = TRUE)

## limit to those with positive vi
walk(files,
     ~ run_cf_vi(.x, keep_pos = TRUE) %>%
         saveRDS(file.path(est_dir,
                           paste0(get_basename(.x), "_vi_pos.rds"))))

## limit those >= various quantiles
walk(files,
     ~ walk(c(0.5, 0.8, 0.9, 0.95),
            ~ run_cf_vi(.y, keep_quant = .x) %>%
                saveRDS(file.path(est_dir,
                                  paste0(get_basename(.y),
                                         "_vi_q",
                                         as.character(.x * 100),
                                         ".rds"))),
            .y = .x))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
