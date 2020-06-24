################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] pull_figure_estimates.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 14 January 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, "..", args)
est_dir <- file.path(root, "estimates")
scr_dir <- file.path(root, "scripts")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("broom","tidyverse","grf"))

## header message
proj_message("pull_figure_estimates.r","h")

## -----------------------------------------------
## utility function
## -----------------------------------------------

get_figure_estimates <- function(cf,
                                 est_name,
                                 subset_str = NULL,
                                 target_sample = "overlap") {
    if (!is.null(subset_str)) {
        x <- cf$X.orig
        subset_expr <- eval(parse(text = subset_str))
        n <- nrow(x[subset_expr,])
        if (length(subset_expr) == 1 && is.na(subset_expr)) {
            subset_expr <- NULL
        }
    } else {
        subset_expr <- NULL
        n <- nrow(cf$X.orig)
    }
    ate <- grf::average_treatment_effect(cf,
                                         target.sample = target_sample,
                                         subset = subset_expr)
    df <- tibble(group = est_name,
                 est = ate[["estimate"]],
                 se = ate[["std.err"]],
                 obs = n)
    return(df)
}

## -----------------------------------------------
## read in causal forest objects
## -----------------------------------------------

proj_message("Reading in causal forest objects")
files <- list.files(file.path(est_dir), "*.rds", full.names = TRUE)

## -----------------------------------------------
## pre-specify estimates
## -----------------------------------------------

## list := c(estimate name, subset string) ...b/c R doesn't have proper tuple
## NB: assume matrix called 'x' for subset
sub_strs <- list(
    c('overall', NULL),
    ## gender
    c('men', "x[,'x1sex.1'] == 1"),
    c('women', "x[,'x1sex.2'] == 1"),
    ## race
    c('ameri', "x[,'x1race.1'] == 1"),
    c('asian', "x[,'x1race.2'] == 1"),
    c('black', "x[,'x1race.3'] == 1"),
    c('hispc', "x[,'x1race.4'] == 1"),
    c('multp', "x[,'x1race.6'] == 1"),
    c('natpi', "x[,'x1race.7'] == 1"),
    c('white', "x[,'x1race.8'] == 1"),
    c('nonwhite', "x[,'x1race.8'] == 0"),
    ## poverty
    c('pov185b', "x[,'x1poverty185.1'] == 1"),
    c('pov185a', "x[,'x1poverty185.0'] == 1"),
    ## gender X race
    c('ameri_m', "x[,'x1sex.1'] == 1 & x[,'x1race.1'] == 1"),
    c('asian_m', "x[,'x1sex.1'] == 1 & x[,'x1race.2'] == 1"),
    c('black_m', "x[,'x1sex.1'] == 1 & x[,'x1race.3'] == 1"),
    c('hispc_m', "x[,'x1sex.1'] == 1 & x[,'x1race.4'] == 1"),
    c('multp_m', "x[,'x1sex.1'] == 1 & x[,'x1race.6'] == 1"),
    c('natpi_m', "x[,'x1sex.1'] == 1 & x[,'x1race.7'] == 1"),
    c('white_m', "x[,'x1sex.1'] == 1 & x[,'x1race.8'] == 1"),
    c('nonwhite_m', "x[,'x1sex.1'] == 1 & x[,'x1race.8'] == 0"),
    c('ameri_f', "x[,'x1sex.2'] == 1 & x[,'x1race.1'] == 1"),
    c('asian_f', "x[,'x1sex.2'] == 1 & x[,'x1race.2'] == 1"),
    c('black_f', "x[,'x1sex.2'] == 1 & x[,'x1race.3'] == 1"),
    c('hispc_f', "x[,'x1sex.2'] == 1 & x[,'x1race.4'] == 1"),
    c('multp_f', "x[,'x1sex.2'] == 1 & x[,'x1race.6'] == 1"),
    c('natpi_f', "x[,'x1sex.2'] == 1 & x[,'x1race.7'] == 1"),
    c('white_f', "x[,'x1sex.2'] == 1 & x[,'x1race.8'] == 1"),
    c('nonwhite_f', "x[,'x1sex.2'] == 1 & x[,'x1race.8'] == 0"),
    ## gender X poverty
    c('pov185b_m', "x[,'x1sex.1'] == 1 & x[,'x1poverty185.1'] == 1"),
    c('pov185b_f', "x[,'x1sex.2'] == 1 & x[,'x1poverty185.1'] == 1"),
    c('pov185a_m', "x[,'x1sex.1'] == 1 & x[,'x1poverty185.0'] == 1"),
    c('pov185a_f', "x[,'x1sex.2'] == 1 & x[,'x1poverty185.0'] == 1"),
    ## race X poverty
    c('pov185b_ameri', "x[,'x1poverty185.1'] == 1 & x[,'x1race.1'] == 1"),
    c('pov185b_asian', "x[,'x1poverty185.1'] == 1 & x[,'x1race.2'] == 1"),
    c('pov185b_black', "x[,'x1poverty185.1'] == 1 & x[,'x1race.3'] == 1"),
    c('pov185b_hispc', "x[,'x1poverty185.1'] == 1 & x[,'x1race.4'] == 1"),
    c('pov185b_multp', "x[,'x1poverty185.1'] == 1 & x[,'x1race.6'] == 1"),
    c('pov185b_natpi', "x[,'x1poverty185.1'] == 1 & x[,'x1race.7'] == 1"),
    c('pov185b_white', "x[,'x1poverty185.1'] == 1 & x[,'x1race.8'] == 1"),
    c('pov185b_nonwhite', "x[,'x1poverty185.1'] == 1 & x[,'x1race.8'] == 0"),
    c('pov185a_ameri', "x[,'x1poverty185.0'] == 1 & x[,'x1race.1'] == 1"),
    c('pov185a_asian', "x[,'x1poverty185.0'] == 1 & x[,'x1race.2'] == 1"),
    c('pov185a_black', "x[,'x1poverty185.0'] == 1 & x[,'x1race.3'] == 1"),
    c('pov185a_hispc', "x[,'x1poverty185.0'] == 1 & x[,'x1race.4'] == 1"),
    c('pov185a_multp', "x[,'x1poverty185.0'] == 1 & x[,'x1race.6'] == 1"),
    c('pov185a_natpi', "x[,'x1poverty185.0'] == 1 & x[,'x1race.7'] == 1"),
    c('pov185a_white', "x[,'x1poverty185.0'] == 1 & x[,'x1race.8'] == 1"),
    c('pov185a_nonwhite', "x[,'x1poverty185.0'] == 1 & x[,'x1race.8'] == 0"),
    ## poverty X gender X race
    ## ...men below poverty line
    c('pov185b_m_ameri', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.1'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_asian', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.2'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_black', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.3'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_hispc', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.4'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_multp', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.6'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_natpi', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.7'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_white', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.8'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185b_m_nonwhite', paste("x[,'x1poverty185.1'] == 1",
                                  "&",
                                  "x[,'x1race.8'] == 0",
                                  "&",
                                  "x[,'x1sex.1'] == 1")),
    ## ...men above poverty line
    c('pov185a_m_ameri', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.1'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_asian', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.2'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_black', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.3'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_hispc', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.4'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_multp', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.6'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_natpi', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.7'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_white', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.8'] == 1",
                               "&",
                               "x[,'x1sex.1'] == 1")),
    c('pov185a_m_nonwhite', paste("x[,'x1poverty185.0'] == 1",
                                  "&",
                                  "x[,'x1race.8'] == 0",
                                  "&",
                                  "x[,'x1sex.1'] == 1")),
    ## ...women below poverty line
    c('pov185b_f_ameri', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.1'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_asian', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.2'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_black', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.3'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_hispc', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.4'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_multp', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.6'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_natpi', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.7'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_white', paste("x[,'x1poverty185.1'] == 1",
                               "&",
                               "x[,'x1race.8'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185b_f_nonwhite', paste("x[,'x1poverty185.1'] == 1",
                                  "&",
                                  "x[,'x1race.8'] == 0",
                                  "&",
                                  "x[,'x1sex.2'] == 1")),
    ## ...women above poverty line
    c('pov185a_f_ameri', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.1'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_asian', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.2'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_black', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.3'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_hispc', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.4'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_multp', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.6'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_natpi', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.7'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_white', paste("x[,'x1poverty185.0'] == 1",
                               "&",
                               "x[,'x1race.8'] == 1",
                               "&",
                               "x[,'x1sex.2'] == 1")),
    c('pov185a_f_nonwhite', paste("x[,'x1poverty185.0'] == 1",
                                  "&",
                                  "x[,'x1race.8'] == 0",
                                  "&",
                                  "x[,'x1sex.2'] == 1"))
)

## -----------------------------------------------
## pull estimates
## -----------------------------------------------

## init test calibration list
tc_list <- vector("list", length = length(files))

proj_message("Pulling, munging, and saving estimates from:")
for (f in files) {

    ## read in forest object
    cf <- readRDS(f)

    ## test calibration
    tc_list[[match(f, files)]] <- test_calibration(cf)

    ## get basename for easy label and save
    bn <- get_basename(f)
    proj_message(bn,"s")

    ## pull estimates for subgroups in data
    ests_sgs <- map(sub_strs,
                    ~ get_figure_estimates(cf,
                                           est_name = .x[1],
                                           subset_str = .x[2])) %>%
        bind_rows

    ## pull estimates based on propensity for enrollment
    prop <- cf$W.hat
    orig <- cf$W.orig
    prop_cut <- cut(prop, c(0,.5,.6,.7,.8,.85,.9,.925,.95,.975,1))
    ests_cut <- map(1:length(unique(prop_cut)),
                    ~ {
                        en <- paste0("cut_", levels(prop_cut)[.x])
                        ss <- paste0("(as.integer(prop_cut) == ", .x, ")")
                        get_figure_estimates(cf,
                                             est_name = en,
                                             subset_str = ss)
                    }) %>%
        bind_rows

    ## combine estimates and compute 95% CI
    ests <- bind_rows(ests_sgs, ests_cut) %>%
        mutate(hi95 = est + (qnorm(.975) * se),
               lo95 = est - (qnorm(.975) * se),
               outcome = bn)
    ## write
    write_csv(ests, file.path(est_dir, paste0(bn, "_est.csv")))
}

proj_message("Saving test calibration results")
## convert test calibration list to tidy tibble
tc_df <- tc_list %>%
    setNames(get_basename(files)) %>%
    map2(.x = .,
         .y = names(.),
         ~ tidy(.x) %>%
             mutate(model = .y)) %>%
    bind_rows %>%
    select(model, everything())

## write
write_csv(tc_df, file.path(est_dir, "test_calibration.csv"))

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
