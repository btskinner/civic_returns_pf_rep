################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] get_data.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 14 January 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, "..", args)
dat_dir <- file.path(root, "data")
raw_dir <- file.path(dat_dir, "raw")
scr_dir <- file.path(root, "scripts")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse", "httr"))

## header message
proj_message("get_data.r","h")

## -----------------------------------------------
## URL + ZIP
## -----------------------------------------------

zip <- "HSLS_2016_v1_0_Stata_Datasets.zip"
url <- file.path("https://nces.ed.gov/EDAT/Data/Zip/", zip)

## -----------------------------------------------
## check if zip exists locally; download if:
## 1. doesn"t exist locally
## 2. different hash
## -----------------------------------------------

## (1) check if exists
is_local <- file.exists(file.path(raw_dir, zip))

## (2) check hash (NA if missing)
md5 <- tools::md5sum(file.path(raw_dir, zip))

## download if necessary; otherwise move forward
if (is_local) {
    if (!is.na(md5) && md5 == "3adcdc4f8de791238e3ec17dfe32998e") {
        proj_message("File already downloaded!")
    }
} else {
    ## download
    proj_message(paste0("Downloading ", zip, " from NCES server."))
    resp <- GET(url,
                write_disk(file.path(raw_dir, zip),
                           overwrite = TRUE),
                progress())

    ## check status code is okay
    if (http_error(resp)) {
        stop("Error downloading file. Try downloading directly from NCES.")
    }
}

## -----------------------------------------------
## unzip file
## -----------------------------------------------

unzip(file.path(raw_dir, zip), exdir = raw_dir)

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
