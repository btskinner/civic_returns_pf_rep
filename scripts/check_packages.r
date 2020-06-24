################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] check_packages.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 19 April 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, '..', args)
scr_dir <- file.path(root, 'scripts')

## source utils
source(file.path(scr_dir, 'utils.r'))

proj_message('check_packages.r','h')

## packages required for this project
req_packages <- c('tidyverse',
                  'caret',
                  'extrafont',
                  'ggpubr',
                  'ggrepel',
                  'grf',
                  'haven',
                  'Hmisc',
                  'httr',
                  'parallel',
                  'Tmisc',
                  'xtable')

## packages that are not installed
mask <- (req_packages %in% installed.packages()[,'Package'])
miss_packages <- req_packages[!mask]

## install any missing
if (length(miss_packages)) {
    proj_message('Installing missing packages')
    install.packages(miss_packages)
} else {
    proj_message('All required packages found')
}

## make sure we import
have_times_font <- grepl('Times', extrafont::fonts())
if (length(have_times_font) == 0 || !have_times_font) {
    proj_message('Importing Times typeface for paper graphics')
    extrafont::font_import(prompt = FALSE, pattern = 'Times')
}

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
