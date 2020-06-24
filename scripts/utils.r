################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] utils.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 12 March 2019
##
################################################################################

## -----------------------------------------------------------------------------
## SETTINGS
## -----------------------------------------------------------------------------

options(tidyverse.quiet = TRUE)

## -----------------------------------------------------------------------------
## FUNCTIONS
## -----------------------------------------------------------------------------

## quiet package load
quiet_require <- function(pkgs) {
    suppressPackageStartupMessages(
        invisible(
            sapply(pkgs, require, character.only = TRUE, quietly = TRUE)
        )
    )
}

## project messages
proj_message <- function(text, type = 'm') {
    w <- 75
    if (type == 'h') {
        message('> ', strwrap(text, width = w))
    } else if (type == 's') {
        message(' --- ', strwrap(text, width = w))
    } else {
        message(' - ', strwrap(text, width = w))
    }
}

## quick function to get basename of file (remove path and extension)
## e.g. > ../path/filename.csv --> filename
get_basename <- function(x) basename(tools::file_path_sans_ext(x))

## take a number, break it into its digits, and make them negative
## e.g. > 1234 --> c(-1,-2,-3,-4); 789 --> c(-7,-8,-9)
int_to_neg_vec <- function(x) {
    as.integer(unlist(str_split(abs(as.numeric(x)),''))) * -1
}

## replace missing values based on lookup table
## e.g.
## > X1MOMOCC2 missing values are -7,-8 according to codebook
## > lookup table (cw) has 78 as reserve code
## > filter lookup table to X1MOMOCC2 row and convert 78 --> c(-7,-8)
## > ifelse() replace missing
val_to_na <- function(df, var, cw) {
    miss_vals <- filter(cw, varname == var) %>% pull(reserve) %>% int_to_neg_vec
    ifelse(as.numeric(df[[var]]) %in% miss_vals, NA, df[[var]])
}

## convert a vector to a factor (exclude = NULL means NA becomes level <NA>)
var_to_factor <- function(x, exclude = NULL) { factor(x, exclude = exclude) }

## garbage collect w/o message
quiet_gc <- function(quiet = TRUE, ...) {
    if (quiet) invisible(gc()) else gc(...)
}

## for plotting since case_when doesn't return NULL values
na_to_null <- function(x) if (is.na(x)) return(NULL) else return(x)

## quick function to calculate p-value from coefficient and its se (two-tail)
pval_func <- function(est, se) {
    pval <- (1 - pnorm(abs(est/se)))*2
    out <- ifelse(pval < 0.01, '<0.01', round(pval,2))
}

## quick pastes
`%+%` <- function(a,b) paste(a,b,sep = ' ')
`%&%` <- function(a,b) paste(a,b,sep = ' & ')
`%|%` <- function(a,b) paste(a,b,sep = ' | ')


## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
