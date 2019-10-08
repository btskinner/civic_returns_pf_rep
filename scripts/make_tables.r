################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] make_tables.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 16 April 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, '..', args)
dat_dir <- file.path(root, 'data')
raw_dir <- file.path(dat_dir, 'raw')
cln_dir <- file.path(dat_dir, 'clean')
est_dir <- file.path(root, 'estimates')
tab_dir <- file.path(root, 'tables')
scr_dir <- file.path(root, 'scripts')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','haven','xtable'))

## header message
proj_message('make_tables.r','h')

## -----------------------------------------------
## read in codebook for variable selection
## -----------------------------------------------

proj_message('Reading and munging variable selection data')
cw <- read_csv(file.path(dat_dir, 'varlist.csv'),
               col_types = cols(varname = 'c',
                                varlabel = 'c',
                                vartype = 'c',
                                round = 'c',
                                type = 'c',
                                restricted = 'c',
                                .default = 'i')
               ) %>%
    filter(keep == 1) %>%
    filter(!(varname %in% c('STU_ID','X4UNIV1','W1STUDENT',
                            'S3CLASSES','S4MARITALSTAT',
                            'S4CHILDREN','S4HRSVOLUNTR',
                            'S4REGVOTE')))
## get vector of factor vars
factor_vars <- cw %>%
    filter(is_factor == 1) %>%
    pull(varname)

## finish subset
cw <- cw %>%
    select(varname, varlabel) %>%
    mutate(varlabel = gsub('%', '\\\\%', varlabel))

## -----------------------------------------------
## table for variables used across models
## -----------------------------------------------

proj_message('Reading and munging variables used in each causal forest')

## munge function
get_var_use <- function(fn, var_cw) {
    ## init data frame
    out_df <- var_cw[,1]
    ## read in CFs
    cf_m <- readRDS(file.path(est_dir, paste0(fn, '_cf.rds')))
    cf_v <- readRDS(file.path(est_dir, paste0(fn, '_cf_vi.rds')))
    ## get variables used (just the main var, not individual factors)
    cf_m_vars <- toupper(unique(gsub('(.*)\\..*', '\\1', names(cf_m$X.orig))))
    cf_v_vars <- toupper(unique(gsub('(.*)\\..*', '\\1', names(cf_v$X.orig))))
    alwa_vars <- c('X1SEX','X1RACE','X1POVERTY185')
    ## add column to data frame
    out_df <- out_df %>%
        mutate(!!fn := ifelse(varname %in% cf_m_vars, 1, 0),
               !!fn := ifelse(varname %in% cf_v_vars, 2, !!sym(fn)),
               !!fn := ifelse(varname %in% alwa_vars, 3, !!sym(fn)))
    ## return
    return(out_df)
}

## get files for table
files <- list.files(file.path(est_dir), '*.rds', full.names = TRUE)
files <- grep('cfw', files, value = TRUE, invert = TRUE)
fname <- get_basename(files) %>% substr(.,1,7) %>% unique
fname <- c('vot_reg','vol_ind','job_bal','job_con')

to_name <- function(fname) {
    case_when(
        fname == 'vot_reg' ~ 'Voter registration',
        fname == 'vol_ind' ~ 'Volunteering',
        fname == 'job_bal' ~ 'Job balance',
        fname == 'job_con' ~ 'Job contribution')
}
to_tex_col <- function(fname_vec) {
    paste(to_name(fname_vec), collapse = '&')
}
convert_func <- function(x) {
    ifelse(x == '1', '$\\cdot$',
    ifelse(x == '2', 'X',
    ifelse(x == '3', 'O', '')))
}
to_italic <- function(x) { gsub('(.+)', '{\\\\itshape \\1}', x) }
add_star <- function(x) { gsub('(.+)', '\\1$^*$', x) }

## make data frame with 1s for used vars and 2s for VI vars
out_df <- map(fname,
              ~ get_var_use(.x, cw)) %>%
    reduce(left_join, by = 'varname') %>%
    filter_at(vars(-varname), any_vars(. != 0))

## make matrix
out_mat <- as.matrix(out_df)
out_mat[,-1] <- apply(out_mat[,-1], 2, convert_func)

## add italics to factor vars
index <- out_mat[,1] %in% factor_vars
out_mat[index,1] <- add_star(out_mat[index,1])

## output file name
fn <- 'predmod.tex'

## table note
note <- c('Initial propensity forest models for each outcome included all ',
          'predictors listed in the table. ',
          'Factor predictors, which were converted to sets of binary ',
          'indicators, are marked with an asterisk. {\\itshape O}s ',
          'represent subgroup predictors; {\\itshape X}s are the ten ',
          'most important predictors (exclusive of subgroup predictors) ',
          'from each initial estimation. Results presented in the paper ',
          'come from propensity forest estimations using only these two ',
          'sets of predictors for each outcome.')


## header
header <- c('\\begin{longtable}{lcccc}',
            '\\caption{Predictors used across models} \\\\',
            '\\label{tab:predmod} \\\\',
            '\\toprule',
            paste0('& ', to_tex_col(fname),'\\\\'),
            '\\midrule',
            '\\endfirsthead',
            '\\multicolumn{5}{l}{\\emph{...table \\thetable{} continued}} \\\\',
            '\\toprule',
            paste0('& ', to_tex_col(fname),'\\\\'),
            '\\midrule',
            '\\endhead',
            '\\bottomrule',
            '\\multicolumn{5}{r}{\\emph{Continued on next page...}} \\\\',
            '\\endfoot',
            '\\endlastfoot')

## footer
footer <- c('\\bottomrule',
            '\\multicolumn{5}{p{.98\\linewidth}}{\\footnotesize ',
            '{\\bfseries Note.} ',
            note,
            '}',
            '\\end{longtable}')

## core content
out <- print(xtable(out_mat),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = 'longtable',
             floating = FALSE,
             caption.placement = 'top',
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message('Saving causal forest variable tables at: ', tab_dir)
proj_message(fn,'s')
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## table for predictor names
## -----------------------------------------------

## output file name
fn <- 'predlab.tex'

## subset to variables used in any model
used_vars <- pull(out_df, varname)
cw_out <- filter(cw, varname %in% used_vars) %>%
    mutate(varlabel = gsub('^X[0-9] ', '', varlabel),
           varlabel = gsub('^S[0-9] ', '', varlabel))

note <- c('Predictor names and labels come directly from the HSLS 2009 ',
          'variable list file found at the National Center for Education ',
          'Statistics website: ',
          'https://nces.ed.gov/surveys/hsls09/hsls09\\_data.asp.')

## header
header <- c('\\begin{longtable}{>{\\RR}p{1.5in}>{\\hangindent=2em}p{4in}}',
            '\\caption{Predictor names and descriptions} \\\\',
            '\\label{tab:predlab} \\\\',
            '\\toprule',
            'Predictor name & Predictor description \\\\',
            '\\midrule',
            '\\endfirsthead',
            '\\multicolumn{2}{l}{\\emph{...table \\thetable{} continued}} \\\\',
            '\\toprule',
            'Predictor name & Predictor description \\\\',
            '\\midrule',
            '\\endhead',
            '\\bottomrule',
            '\\multicolumn{2}{r}{\\emph{Continued on next page...}} \\\\',
            '\\endfoot',
            '\\endlastfoot')

## footer
footer <- c('\\bottomrule',
            '\\multicolumn{2}{p{.98\\linewidth}}{\\footnotesize',
            '{\\bfseries Note.} ',
            note,
            '}',
            '\\end{longtable}')

## core content
out <- print(xtable(as.matrix(cw_out)),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = 'longtable',
             floating = FALSE,
             caption.placement = 'top',
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message('Saving selection table at: ', tab_dir)
proj_message(fn,'s')
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## clean up
## -----------------------------------------------

rm(cw)
quiet_gc()

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
