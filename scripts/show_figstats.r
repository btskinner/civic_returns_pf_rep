################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] show_figstats.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 11 July 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, '..', args)
est_dir <- file.path(root, 'estimates')
scr_dir <- file.path(root, 'scripts')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','grf'))

## -----------------------------------------------
## plot: discrete bar plots
## -----------------------------------------------

df_d <- readRDS(file.path(des_dir, 'desc_discrete.rds')) %>%
    filter(outcome %in% c('vot_reg','vol_ind','job_bal','job_con'),
           type == 'unweighted') %>%
    select(-type) %>%
    mutate(value = case_when(
               outcome %in% c('job_bal','job_con') & value == -1 ~ 2,
               outcome %in% c('job_bal','job_con') & value == 0 ~ 3,
               outcome %in% c('job_bal','job_con') & value == 1 ~ 4,
               TRUE ~ value
           ))

## show
df_d %>% data.frame

## -----------------------------------------------
## read in analysis estimates
## -----------------------------------------------

files <- list.files(file.path(est_dir), '*.csv', full.names = TRUE)
fname <- get_basename(files) %>% gsub('_est', '', .)
df <- map(files, ~ read_csv(.x,
                            col_types = cols(group = 'c',
                                             outcome = 'c',
                                             .default = 'd'))) %>%
    bind_rows

## -----------------------------------------------
## plot: overall
## -----------------------------------------------

outcomes <- paste0(c('vot_reg','vol_ind','job_bal','job_con'),'_cf_vi')

df %>% filter(outcome %in% outcomes, group == 'overall')

## -----------------------------------------------
## plot: gender X race
## -----------------------------------------------

subgroups <- grep('*_f$|*_m$', distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep('pov|non', subgroups, value = TRUE, invert = TRUE)

## vote/volunteering
df %>% filter(outcome == 'vot_reg_cf_vi', group %in% subgroups)
df %>% filter(outcome == 'vol_ind_cf_vi', group %in% subgroups)

## job
df %>% filter(outcome == 'job_bal_cf_vi', group %in% subgroups)
df %>% filter(outcome == 'job_con_cf_vi', group %in% subgroups)

## -----------------------------------------------
## plot: poverty X race
## -----------------------------------------------

subgroups <- grep('^pov', distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep('_m_|_f_|_m$|_f$|non', subgroups, value = TRUE, invert = TRUE)
subgroups <- grep('_', subgroups, value = TRUE, invert = FALSE)

## vote/volunteering
df %>% filter(outcome == 'vot_reg_cf_vi', group %in% subgroups)
df %>% filter(outcome == 'vol_ind_cf_vi', group %in% subgroups)

## job
df %>% filter(outcome == 'job_bal_cf_vi', group %in% subgroups)
df %>% filter(outcome == 'job_con_cf_vi', group %in% subgroups)

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
