################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] fit_regressions.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 14 June 2020
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
quiet_require(c("tidyverse"))

## header message
proj_message("fit_regressions.r","h")

## -----------------------------------------------
## read in data
## -----------------------------------------------

proj_message("Reading in analysis data")
df <- readRDS(file.path(cln_dir, "analysis.rds"))

## -----------------------------------------------
## regressions
## -----------------------------------------------

proj_message("Setting up outcomes, treatment, and drop vars")

## outcomes
outcomes <- c("o_vot_reg", "o_vol_ind", "o_vol_log")

## formulas (rhs)
f1 <- paste(c("t_enrolcol_ever", "x1sex", "x1race", "x1poverty185", "x1ses", "x1region"),
            collapse = "+")
f2 <- paste(c("t_enrolcol_ever:x1sex", "x1race", "x1poverty185", "x1ses", "x1region"),
            collapse = "+")
f3 <- paste(c("t_enrolcol_ever:x1race", "x1sex", "x1poverty185", "x1ses", "x1region"),
            collapse = "+")
f4 <- paste(c("t_enrolcol_ever:x1poverty185", "x1sex", "x1race", "x1ses", "x1region"),
            collapse = "+")
f5 <- paste(c("t_enrolcol_ever:x1race:x1sex", "x1poverty185", "x1ses", "x1region"),
            collapse = "+")
f6 <- paste(c("t_enrolcol_ever:x1race:x1poverty185", "x1sex", "x1ses", "x1region"),
            collapse = "+")
rhs <- list(f1, f2, f3, f4, f5, f6)

## run regressions
fits <- map(outcomes,
            ~ map(rhs,
                  ~ lm(as.formula(paste(.y, .x, sep = "~")), df),
                  .y = .x)) %>%
    setNames(outcomes)

## -----------------------------------------------
## save
## -----------------------------------------------

saveRDS(fits, file.path(est_dir, "regressions.RDS"))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(df)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
