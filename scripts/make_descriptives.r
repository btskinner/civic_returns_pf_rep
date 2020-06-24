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
root <- ifelse(length(args) == 0, "..", args)
dat_dir <- file.path(root, "data")
cln_dir <- file.path(dat_dir, "clean")
scr_dir <- file.path(root, "scripts")
des_dir <- file.path(root, "descriptives")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse", "haven"))

## header message
proj_message("make_descriptives.r", "h")

## -----------------------------------------------
## read in data
## -----------------------------------------------

proj_message("Reading in analysis data")
df <- readRDS(file.path(cln_dir, "analysis.rds"))

## -----------------------------------------------
## get outcomes and treatment variable names
## -----------------------------------------------

## outcomes
outcomes <- names(df) %>% str_subset("^o_")

## treatment
treatment <- names(df) %>% str_subset("^t_")

## -----------------------------------------------
## get mean/median/sd for continuous outcomes
## -----------------------------------------------

## continuous outcomes
outcomes_c <- outcomes %>% str_subset("vot|ind", negate = TRUE)

out_c <- map(outcomes_c,
             ~ df %>%
                 filter(!is.na(.data[[treatment]]),
                        !is.na(.data[[.x]])) %>%
                 select(.data[[treatment]], .data[[.x]]) %>%
                 group_by(.data[[treatment]]) %>%
                 summarise(n = n(),
                           y = mean(.data[[.x]])) %>%
                 ungroup() %>%
                 mutate(outcome = .x)) %>%
    bind_rows %>%
    select(outcome, .data[[treatment]], n, y)

## save tidy data for making figures
proj_message("Saving continuous outcome descriptives data")
saveRDS(out_c, file.path(des_dir, "desc_continuous.rds"))

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
    ungroup

## save tidy data for making figures
proj_message("Saving discrete outcome descriptives data")
saveRDS(out_d, file.path(des_dir, "desc_discrete.rds"))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
