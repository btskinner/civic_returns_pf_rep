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
proj_message <- function(text, type = "m") {
    w <- 75
    if (type == "h") {
        message("> ", strwrap(text, width = w))
    } else if (type == "s") {
        message(" --- ", strwrap(text, width = w))
    } else {
        message(" - ", strwrap(text, width = w))
    }
}

## plyr function to round to arbitrary number
round_any <- function(x, accuracy, f = round){f(x/ accuracy) * accuracy}

## quick function to get basename of file (remove path and extension)
## e.g. > ../path/filename.csv --> filename
get_basename <- function(x) basename(tools::file_path_sans_ext(x))

## take a number, break it into its digits, and make them negative
## e.g. > 1234 --> c(-1,-2,-3,-4); 789 --> c(-7,-8,-9)
int_to_neg_vec <- function(x) {
    as.integer(unlist(stringr::str_split(abs(as.numeric(x)),""))) * -1
}

## replace missing values based on lookup table
## e.g.
## > X1MOMOCC2 missing values are -7,-8 according to codebook
## > lookup table (cw) has 78 as reserve code
## > filter lookup table to X1MOMOCC2 row and convert 78 --> c(-7,-8)
## > replace with NA: df[as.numeric(df[[.x]]) %in% miss_vals, .x] <- NA
val_to_na <- function(df, vars, cw, cw_var_col, cw_res_col) {
    ## get column names for later ordering
    all <- names(df)
    ## pull out non-missing columns
    tmp <- select(df, -all_of(vars))
    ## replace reserved values with NA, column by column; bind into tibble
    fix <- map_dfc(vars,
                   ~ {
                       ## get missing values from crosswalk
                       miss_vals <- filter(cw, {{ cw_var_col }} == .x) %>%
                           pull({{ cw_res_col }}) %>%
                           int_to_neg_vec
                       ## replace missing with NA
                       df[as.numeric(df[[.x]]) %in% miss_vals, .x] <- NA
                       ## return fixed vector
                       df[[.x]]
                   }) %>%
        ## replace `V#` column names with actual names
        setNames(vars)
    ## rebind non-missing and fixed columns; reorder
    bind_cols(tmp, fix) %>% select(all_of(all))
}

## convert a vector to a factor (exclude = NULL means NA becomes level <NA>)
var_to_factor <- function(x, exclude = NULL) { factor(x, exclude = exclude) }

## convert vector of effectively ordered numbers into ordered,
## continuous categories
## e.g.
## > c(0, 1000, 5000, 200) --> c(1, 3, 4, 2)
ordcat_to_rank <- function(x) { match(x, sort(unique(x))) }

## garbage collect w/o message
quiet_gc <- function(quiet = TRUE, ...) {
    if (quiet) invisible(gc()) else gc(...)
}

## for plotting since case_when doesn't return NULL values
na_to_null <- function(x) if (is.na(x)) return(NULL) else return(x)

## quick function to calculate p-value from coefficient and its se (two-tail)
pval_func <- function(est, se) {
    pval <- (1 - pnorm(abs(est/se)))*2
    out <- ifelse(pval < 0.01, "<0.01", round(pval,2))
}

## quick pastes
`%+%` <- function(a,b) paste(a,b,sep = " ")
`%&%` <- function(a,b) paste(a,b,sep = " & ")
`%|%` <- function(a,b) paste(a,b,sep = " | ")

## make a vector into a tex column
to_tex_col <- function(fname_vec) {
    paste(to_table_names(fname_vec), collapse = "&")
}

## convert nums to table symbols
convert_func <- function(x) {
    ifelse(x == "1", "$\\cdot$",
    ifelse(x == "2", "X",
    ifelse(x == "3", "O", "")))
}

## add italics
to_italic <- function(x) { gsub("(.+)", "{\\\\itshape \\1}", x) }

## add parentheses
add_paren <- function(x) { gsub("(.+)", "(\\1)", x) }

## add indentation (\hspace)
add_hspace <- function(x, em) {
    gsub("(.+)", paste0("\\\\hspace{", em, "em}\\1"), x)
}

## add one star
add_star <- function(x) { gsub("(.+)", "\\1$^*$", x) }

## add multiple stars based on p-value
add_stars <- function(x, y) {
    y <- as.numeric(y)
    case_when(
        y <= 0.05 & y > 0.01 ~ gsub("(.+)", "\\1$^{*}$", x),
        y <= 0.01 & y > 0.001 ~ gsub("(.+)", "\\1$^{**}$", x),
        y <= 0.001 ~ gsub("(.+)", "\\1$^{***}$", x),
        TRUE ~ as.character(x)
    )}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
