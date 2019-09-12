################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] make_descriptives.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 29 June 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, '..', args)
dat_dir <- file.path(root, 'data')
cln_dir <- file.path(dat_dir, 'clean')
scr_dir <- file.path(root, 'scripts')
des_dir <- file.path(root, 'descriptives')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','haven','Hmisc'))

## header message
proj_message('make_descriptives.r','h')

## -----------------------------------------------
## read in data
## -----------------------------------------------

proj_message('Reading in analysis data')
df <- readRDS(file.path(cln_dir, 'analysis.rds'))

## -----------------------------------------------
## get outcomes and treatment variable names
## -----------------------------------------------

## outcomes
outcomes <- c('vot_reg','vol_hrs','vol_ind','job_bal','job_con')

## treatment
treatment <- grep('enrolcol', names(df), value = TRUE)

## sample weights
weights <- grep('^w1', names(df), value = TRUE)

## -----------------------------------------------
## get mean/median/sd for continuous outcomes
## -----------------------------------------------

## continuous outcomes have 'h' in the name for hours
outcomes_c <- grep('h', outcomes, value = TRUE)

## get tidy data frame
out_c <- map(outcomes_c,
             ~ df %>%
                 filter(!is.na(.data[[treatment]]),
                        !is.na(.data[[.x]])) %>%
                 group_by(.data[[treatment]]) %>%
                 mutate(n = n()) %>%
                 group_by(.data[[treatment]], n) %>%
                 summarise_at(.vars = .x,
                              .funs = lst(
                                  mean,
                                  median,
                                  sd,
                                  w_mean = ~ wtd.mean(.,
                                                      weights = .data[[weights]],
                                                      normwt = TRUE,
                                                      na.rm = FALSE),
                                  w_median = ~ wtd.quantile(.,
                                                            weights = .data[[weights]],
                                                            probs = .5,
                                                            normwt = TRUE,
                                                            na.rm = FALSE),
                                  w_sd = ~ sqrt(wtd.var(.,
                                                        weights = .data[[weights]],
                                                        normwt = TRUE,
                                                        na.rm = FALSE)))) %>%
                 ungroup() %>%
                 gather(stat, value, -.data[[treatment]]) %>%
                 mutate(outcome = .x)) %>%
    bind_rows %>%
    select(outcome, .data[[treatment]], stat, value)

## save tidy data for making figures
proj_message('Saving continuous outcome descriptives data')
saveRDS(out_c, file.path(des_dir, 'desc_continuous.rds'))

## -----------------------------------------------
## get crosstabs for discrete outcomes
## -----------------------------------------------

## take difference between continuous sublist and full list
outcomes_d <- setdiff(outcomes, outcomes_c)

## get tidy data frame (unweighted)
out_d <- map(outcomes_d,
             ~ df %>%
                 filter(!is.na(.data[[treatment]]),
                        !is.na(.data[[.x]])) %>%
                 group_by(.data[[treatment]], .data[[.x]]) %>%
                 count() %>%
                 ungroup() %>%
                 mutate(pct = round(n / sum(n) * 100, 2)) %>%
                 gather(outcome, value, -c(.data[[treatment]], n, pct))) %>%
    bind_rows %>%
    select(outcome, value, .data[[treatment]], n, pct) %>%
    group_by(outcome, .data[[treatment]]) %>%
    mutate(n_within_treat = sum(n),
           pct_within_treat = round(n / n_within_treat * 100, 2)) %>%
    ungroup %>%
    mutate(type = 'unweighted')

## get tidy data frame (weighted)
out_d_w <- map(outcomes_d,
               ~ df %>%
                   filter(!is.na(.data[[treatment]]),
                          !is.na(.data[[.x]])) %>%
                   group_by(.data[[treatment]], .data[[.x]]) %>%
                   count(wt = .data[[weights]]) %>%
                   ungroup() %>%
                   mutate(pct = round(n / sum(n) * 100, 2)) %>%
                   gather(outcome, value, -c(.data[[treatment]], n, pct))) %>%
    bind_rows %>%
    select(outcome, value, .data[[treatment]], n, pct) %>%
    group_by(outcome, .data[[treatment]]) %>%
    mutate(n_within_treat = sum(n),
           pct_within_treat = round(n / n_within_treat * 100, 2)) %>%
    ungroup %>%
    mutate(type = 'weighted')

## bind
out_d <- bind_rows(out_d, out_d_w)

## save tidy data for making figures
proj_message('Saving discrete outcome descriptives data')
saveRDS(out_d, file.path(des_dir, 'desc_discrete.rds'))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
