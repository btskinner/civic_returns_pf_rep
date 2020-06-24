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
root <- ifelse(length(args) == 0, "..", args)
dat_dir <- file.path(root, "data")
raw_dir <- file.path(dat_dir, "raw")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path(root, "estimates")
tab_dir <- file.path(root, "tables")
scr_dir <- file.path(root, "scripts")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse","haven","xtable"))

## header message
proj_message("make_tables.r","h")

## -----------------------------------------------------------------------------
## functions
## -----------------------------------------------------------------------------

## munge function
get_var_use <- function(fn, var_cw) {
    ## init data frame
    out_df <- var_cw[,1]
    ## read in CFs
    cf_m <- readRDS(file.path(est_dir, paste0(fn, "_cf.rds")))
    cf_v <- readRDS(file.path(est_dir, paste0(fn, "_cf_vi_q90.rds")))
    ## get variables used (just the main var, not individual factors)
    cf_m_vars <- toupper(unique(gsub("(.*)\\..*", "\\1", names(cf_m$X.orig))))
    cf_v_vars <- toupper(unique(gsub("(.*)\\..*", "\\1", names(cf_v$X.orig))))
    alwa_vars <- c("X1SEX","X1RACE","X1POVERTY185")
    ## add column to data frame
    out_df <- out_df %>%
        mutate(!!fn := ifelse(varname %in% cf_m_vars, 1, 0),
               !!fn := ifelse(varname %in% cf_v_vars, 2, !!sym(fn)),
               !!fn := ifelse(varname %in% alwa_vars, 3, !!sym(fn)))
    ## return
    return(out_df)
}

## to_name <- function(fname) {
##     case_when(
##         fname == "o_vot_reg" ~ "Registered to vote",
##         fname == "o_vol_ind" ~ "Volunteering (indicator)",
##         fname == "o_vol_log" ~ "Log(volunteer hours) ",)
## }
to_table_names <- function(x) {
    case_when(
        x == "overall" ~ "Overall",
        x == "men" ~ "Men",
        x == "women" ~ "Women",
        str_detect(x, "ameri") ~ "American Indian/Alaska Native",
        str_detect(x, "asian") ~ "Asian",
        str_detect(x, "black") ~ "Black",
        str_detect(x, "hispc") ~ "Hispanic",
        str_detect(x, "multp") ~ "More than one race",
        str_detect(x, "natpi") ~ "Native Hawaiian/Pacific Islander",
        str_detect(x, "white") ~ "White",
        x == "pov185b" ~ "Below 185\\% of poverty line",
        x == "pov185a" ~ "Above 185\\% of poverty line",
        x == "cut_(0,0.5]" ~ "(0,0.5]",
        x == "cut_(0.5,0.6]" ~ "(0.5,0.6]",
        x == "cut_(0.6,0.7]" ~ "(0.6,0.7]",
        x == "cut_(0.7,0.8]" ~ "(0.7,0.8]",
        x == "cut_(0.8,0.85]" ~ "(0.8,0.85]",
        x == "cut_(0.85,0.9]" ~ "(0.85,0.9]",
        x == "cut_(0.9,0.925]" ~ "(0.9,0.925]",
        x == "cut_(0.925,0.95]" ~ "(0.925,0.95]",
        x == "cut_(0.95,0.975]" ~ "(0.95,0.975]",
        x == "cut_(0.975,1]" ~ "(0.975,1]",
        x == "o_vol_ind" ~ "Volunteered",
        x == "o_vol_log" ~ "Log(volunteer hours)",
        x == "o_vot_reg" ~ "Registered to vote",
    )
}
to_tex_col <- function(fname_vec) {
    paste(to_table_names(fname_vec), collapse = "&")
}
convert_func <- function(x) {
    ifelse(x == "1", "$\\cdot$",
    ifelse(x == "2", "X",
    ifelse(x == "3", "O", "")))
}
to_italic <- function(x) { gsub("(.+)", "{\\\\itshape \\1}", x) }
add_star <- function(x) { gsub("(.+)", "\\1$^*$", x) }
add_stars <- function(x, y) {
    y <- as.numeric(y)
    case_when(
        y <= 0.05 & y > 0.01 ~ gsub("(.+)", "\\1$^{*}$", x),
        y <= 0.01 & y > 0.001 ~ gsub("(.+)", "\\1$^{**}$", x),
        y <= 0.001 ~ gsub("(.+)", "\\1$^{***}$", x),
        TRUE ~ as.character(x)
    )}
add_paren <- function(x) { gsub("(.+)", "(\\1)", x) }
add_hspace <- function(x, em) {
    gsub("(.+)", paste0("\\\\hspace{", em, "em}\\1"), x)
}

## -----------------------------------------------
## read in codebook for variable selection
## -----------------------------------------------

proj_message("Reading and munging variable selection data")
cw <- read_csv(file.path(raw_dir, "varlist.csv"),
               col_types = cols(varname = "c",
                                varlabel = "c",
                                vartype = "c",
                                round = "c",
                                type = "c",
                                restricted = "c",
                                .default = "i")
               ) %>%
    filter(keep == 1) %>%
    filter(str_detect(varname, "S4|W4|X4", negate = TRUE))

## get vector of factor vars
factor_vars <- cw %>%
    filter(is_factor == 1) %>%
    pull(varname)

## finish subset
cw <- cw %>%
    select(varname, varlabel) %>%
    mutate(varlabel = gsub("%", "\\\\%", varlabel))

## -----------------------------------------------
## table for variables used across models
## -----------------------------------------------

proj_message("Reading and munging variables used in each causal forest")

## get files for table
files <- list.files(file.path(est_dir), "*.rds", full.names = TRUE)
files <- grep("cfw", files, value = TRUE, invert = TRUE)
fname <- get_basename(files) %>% substr(.,1,7) %>% unique
fname <- c("o_vot_reg","o_vol_ind","o_vol_log")

## make data frame with 1s for used vars and 2s for VI vars
out_df <- map(fname,
              ~ get_var_use(.x, cw)) %>%
    reduce(left_join, by = "varname") %>%
    filter_at(vars(-varname), any_vars(. != 0))

## make matrix
out_mat <- as.matrix(out_df)
out_mat[,-1] <- apply(out_mat[,-1], 2, convert_func)

## add italics to factor vars
index <- out_mat[,1] %in% factor_vars
out_mat[index,1] <- add_star(out_mat[index,1])

## output file name
fn <- "predmod.tex"

## table note
note <- c("Initial propensity forest models for each outcome included all ",
          "predictors listed in the table. ",
          "Factor predictors, which were converted to sets of binary ",
          "indicators, are marked with an asterisk. {\\itshape O}s ",
          "represent subgroup predictors; {\\itshape X}s are the ",
          "most important predictors (exclusive of subgroup predictors) ",
          "from each initial estimation. Results presented in the paper ",
          "come from propensity forest estimations using only these two ",
          "sets of predictors for each outcome.")


## header
header <- c("\\begin{longtable}{>{\\RR}p{2in}cccc}",
            "\\caption{Predictors used across models} \\\\",
            "\\label{tab:predmod} \\\\",
            "\\toprule",
            paste0("& ", to_tex_col(fname),"\\\\"),
            "\\midrule",
            "\\endfirsthead",
            "\\multicolumn{5}{l}{\\emph{...table \\thetable{} continued}} \\\\",
            "\\toprule",
            paste0("& ", to_tex_col(fname),"\\\\"),
            "\\midrule",
            "\\endhead",
            "\\bottomrule",
            "\\multicolumn{5}{r}{\\emph{Continued on next page...}} \\\\",
            "\\endfoot",
            "\\endlastfoot")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{5}{p{.98\\linewidth}}{\\footnotesize ",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{longtable}")

## core content
out <- print(xtable(out_mat),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = "longtable",
             floating = FALSE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving causal forest variable tables at: ", tab_dir)
proj_message(fn,"s")
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## table for predictor names
## -----------------------------------------------

## output file name
fn <- "predlab.tex"

## subset to variables used in any model
used_vars <- pull(out_df, varname)
cw_out <- filter(cw, varname %in% used_vars) %>%
    mutate(varlabel = gsub("^X[0-9] ", "", varlabel),
           varlabel = gsub("^S[0-9] ", "", varlabel))

note <- c("Predictor names and labels come directly from the HSLS 2009 ",
          "variable list file found at the National Center for Education ",
          "Statistics website: ",
          "https://nces.ed.gov/surveys/hsls09/hsls09\\_data.asp.")

## header
header <- c("\\begin{longtable}{>{\\RR}p{1.5in}>{\\hangindent=2em}p{4in}}",
            "\\caption{Predictor names and descriptions} \\\\",
            "\\label{tab:predlab} \\\\",
            "\\toprule",
            "Predictor name & Predictor description \\\\",
            "\\midrule",
            "\\endfirsthead",
            "\\multicolumn{2}{l}{\\emph{...table \\thetable{} continued}} \\\\",
            "\\toprule",
            "Predictor name & Predictor description \\\\",
            "\\midrule",
            "\\endhead",
            "\\bottomrule",
            "\\multicolumn{2}{r}{\\emph{Continued on next page...}} \\\\",
            "\\endfoot",
            "\\endlastfoot")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{2}{p{.98\\linewidth}}{\\footnotesize",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{longtable}")

## core content
out <- print(xtable(as.matrix(cw_out)),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = "longtable",
             floating = FALSE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving selection table at: ", tab_dir)
proj_message(fn,"s")
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## table for calibration checks
## -----------------------------------------------

proj_message("Making table for test calibration stats")

## output file name
fn <- "calibration.tex"

## read in data; munging toward a table...
tc <- read_csv(file.path(est_dir, "test_calibration.csv")) %>%
    filter(str_detect(model, "w", negate = TRUE),
           str_detect(model, "reg|ind|log")) %>%
    arrange(model) %>%
    select(-statistic) %>%
    pivot_longer(cols = c("estimate","std.error"),
                 names_to = "stat",
                 values_to = "value") %>%
    mutate(term = case_when(
               str_detect(term, "mean") ~ "mfp",
               str_detect(term, "diff") ~ "dfp"),
           stat = case_when(
               str_detect(stat, "est") ~ "est",
               str_detect(stat, "std") ~ "se")
           ) %>%
    rename(pval = p.value) %>%
    unite(col = "term_stat",
          c("term", "stat"))

## ...continuing munge toward table
out <- bind_cols(tc %>%
                filter(str_detect(term_stat, "mfp")) %>%
                rename(mfp_pval = pval,
                       mfp_val = value) %>%
                select(-term_stat),
                tc %>%
                filter(str_detect(term_stat, "dfp")) %>%
                rename(dfp_pval = pval,
                       dfp_val = value) %>%
                select(-term_stat, -model)) %>%
    select(model, mfp_val, dfp_val, mfp_pval, dfp_pval) %>%
    as.matrix

## masks for rows
even <- seq(2,nrow(out),2)
odds <- seq(1,nrow(out),2)

## add parentheses to standard errors in even rows
out[even, 2:3] <- add_paren(round(as.numeric(out[even, 2:3]), 4))

## add stars to values in odd rows (as necessary)
out[odds, "mfp_val"] <- add_stars(x = round(as.numeric(out[odds, "mfp_val"]), 3),
                                  y = out[odds, "mfp_pval"])
out[odds, "dfp_val"] <- add_stars(x = round(as.numeric(out[odds, "dfp_val"]), 3),
                                  y = out[odds, "dfp_pval"])

## drop pvals
out <- out[, str_detect(colnames(out), "pval", negate = TRUE)]

## make nicer for table
out[even, 1] <- ""
out[, 1] <- case_when(
    str_detect(out[, 1], "o_vot_reg_cf$") ~ paste0("{\\bfseries ",
                                               to_table_names("o_vot_reg"), "}"),
    str_detect(out[, 1], "o_vol_ind_cf$") ~ paste0("{\\bfseries ",
                                               to_table_names("o_vol_ind"), "}"),
    str_detect(out[, 1], "o_vol_log_cf$") ~ paste0("{\\bfseries ",
                                               to_table_names("o_vol_log"), "}"),
    str_detect(out[, 1], "pos") ~ "\\hspace{1em}Positive",
    str_detect(out[, 1], "q50") ~ "\\hspace{1em}50$^{th}$ quantile",
    str_detect(out[, 1], "q80") ~ "\\hspace{1em}80$^{th}$ quantile",
    str_detect(out[, 1], "q90") ~ "\\hspace{1em}90$^{th}$ quantile",
    str_detect(out[, 1], "q95") ~ "\\hspace{1em}95$^{th}$ quantile",
    TRUE ~ ""
)

## add subheading
out[c(2,14,26), 1] <- "{\\itshape Variable importance subset}"

## rearrange so voter registration is first
out <- rbind(out[25:nrow(out),],
             out[1:24,])

## table note
note <- c("Bold rows represent propensity forests fit using all variables. Rows under ",
          "each model represent models run with only most important variables ",
          "that fall within the cut point (any positive value or at/above quantile level ",
          "of importance). ",
          "A significant mean forest prediction estimate of 1 offers evidence that the ",
          "mean forest prediction is correct; a differential forest prediction ",
          "estimate of 1 or greater ",
          "suggests the predictions also capture any underlying heterogeneity. The ",
          "p-value of the differential forest prediction can be understood as test ",
          "of underlying heterogeneity against a null hypothesis of no heterogeneity. ",
          "See {\\tt grf::test\\_calibration()} help file: ",
          paste0("https://grf-labs.github.io/", "grf/reference/test\\_calibration.html"))

## header
header <- c("\\begin{table}",
            paste0("\\caption{Test calibration statistics for each propensity ",
                   "forest fit}"),
            "\\label{tab:calibration}",
            "\\begin{tabularx}{\\textwidth}{Xcc}",
            "\\toprule",
            "Model & Mean forest prediction & Differential forest prediction \\\\",
            "\\midrule")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{3}{p{.98\\linewidth}}{\\footnotesize",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{tabularx}",
            "\\end{table}")

## core content
out <- print(xtable(as.matrix(out)),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving selection table at: ", tab_dir)
proj_message(fn,"s")
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## table for estimates in figures
## -----------------------------------------------

proj_message("Making table for estimates shown in figures")

## output file name
fn <- "estimates.tex"

## get files used in figures
files <- list.files(est_dir, "[reg|ind|log]_cf_vi_q80_est.csv", full.names = TRUE)

mods <- c("reg","ind","log")

out <- map(mods,
           ~ {
               f <- files[str_detect(files, .x)]
               df <- read_csv(f)
               groups <- c("overall",
                           "men",
                           "women",
                           "ameri",
                           "asian",
                           "black",
                           "hispc",
                           "multp",
                           "natpi",
                           "white",
                           "pov185b",
                           "pov185a",
                           "ameri_m",
                           "asian_m",
                           "black_m",
                           "hispc_m",
                           "multp_m",
                           "natpi_m",
                           "white_m",
                           "ameri_f",
                           "asian_f",
                           "black_f",
                           "hispc_f",
                           "multp_f",
                           "natpi_f",
                           "white_f",
                           "pov185a_ameri",
                           "pov185a_asian",
                           "pov185a_black",
                           "pov185a_hispc",
                           "pov185a_multp",
                           "pov185a_natpi",
                           "pov185a_white",
                           "pov185b_ameri",
                           "pov185b_asian",
                           "pov185b_black",
                           "pov185b_hispc",
                           "pov185b_multp",
                           "pov185b_natpi",
                           "pov185b_white",
                           "cut_(0,0.5]",
                           "cut_(0.5,0.6]",
                           "cut_(0.6,0.7]",
                           "cut_(0.7,0.8]",
                           "cut_(0.8,0.85]",
                           "cut_(0.85,0.90]",
                           "cut_(0.90,0.925]",
                           "cut_(0.925,0.95]",
                           "cut_(0.95,0.975]",
                           "cut_(0.975,1]")

               ## put together main table
               out <- df %>%
                   filter(group %in% groups) %>%
                   select(group, est, se, obs) %>%
                   pivot_longer(cols = c("est", "se"),
                                names_to = "stat",
                                values_to = "value") %>%
                   mutate(group = ifelse(row_number() %% 2 == 0,
                                         "",
                                         to_table_names(group)),
                          group =  ifelse(row_number() > 2,
                                          paste0("\\hspace{2em}", group),
                                          paste0("{\\bfseries ", group, "}")),
                          value = ifelse(row_number() %% 2 == 0,
                                         add_paren(round(value, 4)),
                                         round(value, 3)),
                          value = ifelse(row_number() <= 2,
                                         paste0("{\\bfseries ", value, "}"),
                                         value),
                          obs = ifelse(row_number() %% 2 == 0,
                                       "",
                                       obs),
                          obs = ifelse(row_number() <= 2,
                                       paste0("{\\bfseries ", obs, "}"),
                                       obs)) %>%
                   select(group, value, obs)

               if (.x != "reg") {
                   out <- out %>% select(-group)
               }
               out
           }) %>%
    bind_cols() %>%
    as.matrix

## add sections
out <- rbind(out[1:2,],
             cbind("{\\bfseries Single group}",
                   rbind(rep("",6))),
             cbind("\\hspace{1em}{\\bfseries Gender}",
                   rbind(rep("",6))),
             out[3:6,],
             cbind("\\hspace{1em}{\\bfseries Race/ethnicity}",
                   rbind(rep("",6))),
             out[7:20,],
             cbind("\\hspace{1em}{\\bfseries Poverty line}",
                   rbind(rep("",6))),
             out[21:24,],
             cbind("{\\bfseries Gender by race/ethnicity}",
                   rbind(rep("",6))),
             cbind("\\hspace{1em}{\\itshape Men}",
                   rbind(rep("",6))),
             out[25:38,],
             cbind("\\hspace{1em}{\\itshape Women}",
                   rbind(rep("",6))),
             out[39:52,],
             cbind("{\\bfseries Poverty status by race/ethnicity}",
                   rbind(rep("",6))),
             cbind("\\hspace{1em}{\\itshape Below 185\\% of poverty line}",
                   rbind(rep("",6))),
             out[53:66,],
             cbind("\\hspace{1em}{\\itshape Above 185\\% of poverty line}",
                   rbind(rep("",6))),
             out[67:80,],
             cbind("{\\bfseries Propensity of enrollment}",
                   rbind(rep("",6))),
             out[81:96,])


note <- c("$^{***} p < 0.001$; $^{**} p < 0.01$; $^{*} p < 0.05$. ",
          "$ATE$: Average treatment effect; $N$ is sample size. These estimates are ",
          "the same as shown in the figures 2-5, with the exception that standard ",
          "errors (rather than $p$-values) are shown in parentheses.")

## header
header <- c("\\begin{longtable}{>{\\RR}p{2.5in}cccccc}",
            "\\caption{Average treatment effect estimates across subgroups} \\\\",
            "\\label{tab:estimates} \\\\",
            "\\toprule",
            paste0(" & \\multicolumn{2}{c}{Registered to vote}",
                   " & \\multicolumn{2}{c}{Volunteer}",
                   " & \\multicolumn{2}{c}{Log(volunteer hours) }",
                   "\\\\"),
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
            "&ATE&$N$&ATE&$N$&ATE&$N$ \\\\",
            "\\midrule",
            "\\endfirsthead",
            "\\multicolumn{7}{l}{\\emph{...table \\thetable{} continued}} \\\\",
            "\\toprule",
            paste0(" & \\multicolumn{2}{c}{Registered to vote}",
                   " & \\multicolumn{2}{c}{Volunteer}",
                   " & \\multicolumn{2}{c}{Log(volunteer hours)}",
                   "\\\\"),
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
            "&ATE&$N$&ATE&$N$&ATE&$N$ \\\\",
            "\\midrule",
            "\\endhead",
            "\\bottomrule",
            "\\multicolumn{7}{r}{\\emph{Continued on next page...}} \\\\",
            "\\endfoot",
            "\\endlastfoot")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{7}{p{.98\\linewidth}}{\\footnotesize",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{longtable}")

## core content
out <- print(xtable(out),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = "longtable",
             floating = FALSE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving selection table at: ", tab_dir)
proj_message(fn,"s")
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## table for descriptives comparison
## -----------------------------------------------

proj_message("Making table of sample comparison with full data set")

## output file name
fn <- "comparison.tex"

## get data
df_a <- readRDS(file.path(cln_dir, "analysis.rds"))
df_c <- readRDS(file.path(cln_dir, "comparison.rds"))

## -----------------
## full dataset
## include missing
## -----------------

## enroll
t <- df_c %>%
    ## filter(!is.na(t_enrolcol_ever)) %>%
    count(t_enrolcol_ever) %>%
    mutate(prop = n / sum(n),
           t_enrolcol_ever = case_when(
               t_enrolcol_ever == 0 ~ "Non-enroll",
               t_enrolcol_ever == 1 ~ "Enroll",
               is.na(t_enrolcol_ever) ~ "Missing")) %>%
    select(col = t_enrolcol_ever, prop)

## gender
g <- df_c %>%
    count(x1sex) %>%
    mutate(prop = n / sum(n),
           x1sex = case_when(
               x1sex == 1 ~ "Male",
               x1sex == 2 ~ "Female",
               is.na(x1sex) ~ "Missing")) %>%
    select(col = x1sex, prop)

## race/ethnicity
r <- df_c %>%
    count(x1race) %>%
    mutate(prop = n / sum(n),
           x1race = case_when(
               x1race == 1 ~ "American Indian/Alaska Native",
               x1race == 2 ~ "Asian",
               x1race == 3 ~ "Black",
               x1race == 4 ~ "Hispanic",
               x1race == 6 ~ "More than one race",
               x1race == 7 ~ "Native Hawaiian/Pacific Islander",
               x1race == 8 ~ "White",
               is.na(x1race) ~ "Missing")) %>%
    select(col = x1race, prop)

## poverty status
p <- df_c %>%
    count(x1poverty185) %>%
    mutate(prop = n / sum(n),
           x1poverty185 = case_when(
               x1poverty185 == 1 ~ "Below 185\\% of poverty line",
               x1poverty185 == 0 ~ "Above 185\\% of poverty line",
               is.na(x1poverty185) ~ "Missing")) %>%
    select(col = x1poverty185, prop)

## bind together
out_c_1 <- bind_rows(t, g, r, p) %>%
    mutate(prop = as.character(round(prop * 100, 2)))

## -----------------
## full dataset
## drop missing
## -----------------

t <- df_c %>%
    filter(!is.na(t_enrolcol_ever)) %>%
    count(t_enrolcol_ever) %>%
    mutate(prop = n / sum(n),
           t_enrolcol_ever = case_when(
               t_enrolcol_ever == 0 ~ "Non-enroll",
               t_enrolcol_ever == 1 ~ "Enroll",
               is.na(t_enrolcol_ever) ~ "Missing")) %>%
    select(col = t_enrolcol_ever, prop)

## gender
g <- df_c %>%
    filter(!is.na(x1sex)) %>%
    count(x1sex) %>%
    mutate(prop = n / sum(n),
           x1sex = case_when(
               x1sex == 1 ~ "Male",
               x1sex == 2 ~ "Female",
               is.na(x1sex) ~ "Missing")) %>%
    select(col = x1sex, prop)

## race/ethnicity
r <- df_c %>%
    filter(!is.na(x1race)) %>%
    count(x1race) %>%
    mutate(prop = n / sum(n),
           x1race = case_when(
               x1race == 1 ~ "American Indian/Alaska Native",
               x1race == 2 ~ "Asian",
               x1race == 3 ~ "Black",
               x1race == 4 ~ "Hispanic",
               x1race == 6 ~ "More than one race",
               x1race == 7 ~ "Native Hawaiian/Pacific Islander",
               x1race == 8 ~ "White",
               is.na(x1race) ~ "Missing")) %>%
    select(col = x1race, prop)

## poverty status
p <- df_c %>%
    filter(!is.na(x1poverty185)) %>%
    count(x1poverty185) %>%
    mutate(prop = n / sum(n),
           x1poverty185 = case_when(
               x1poverty185 == 1 ~ "Below 185\\% of poverty line",
               x1poverty185 == 0 ~ "Above 185\\% of poverty line",
               is.na(x1poverty185) ~ "Missing")) %>%
    select(col = x1poverty185, prop)

## bind together
out_c_2 <- bind_rows(t, g, r, p) %>%
    mutate(prop = as.character(round(prop * 100, 2)))

## -----------------
## analysis dataset
## -----------------

outcomes <- c("o_vot_reg", "o_vol_ind", "o_vol_log")

out_a <- map(outcomes,
             ~ {
                 tmp <- df_a %>%
                     filter(!is.na(!!sym(.x)))

                 ## enrolle
                 t <- tmp %>%
                     count(t_enrolcol_ever) %>%
                     mutate(prop = n / sum(n),
                            t_enrolcol_ever = case_when(
                                t_enrolcol_ever == 0 ~ "Non-enroll",
                                t_enrolcol_ever == 1 ~ "Enroll",
                                is.na(t_enrolcol_ever) ~ "Missing")) %>%
                     select(col = t_enrolcol_ever, prop)

                 ## gender
                 g <- tmp %>%
                     count(x1sex) %>%
                     mutate(prop = n / sum(n),
                            x1sex = case_when(
                                x1sex == 1 ~ "Male",
                                x1sex == 2 ~ "Female",
                                is.na(x1sex) ~ "Missing")) %>%
                     select(col = x1sex, prop)

                 ## race/ethnicity
                 r <- tmp %>%
                     count(x1race) %>%
                     mutate(prop = n / sum(n),
                            x1race = case_when(
                                x1race == 1 ~ "American Indian/Alaska Native",
                                x1race == 2 ~ "Asian",
                                x1race == 3 ~ "Black",
                                x1race == 4 ~ "Hispanic",
                                x1race == 6 ~ "More than one race",
                                x1race == 7 ~ "Native Hawaiian/Pacific Islander",
                                x1race == 8 ~ "White",
                                is.na(x1race) ~ "Missing")) %>%
                     select(col = x1race, prop)

                 ## poverty status
                 p <- tmp %>%
                     count(x1poverty185) %>%
                     mutate(prop = n / sum(n),
                            x1poverty185 = case_when(
                                x1poverty185 == 1 ~ "Below 185\\% of poverty line",
                                x1poverty185 == 0 ~ "Above 185\\% of poverty line",
                                TRUE ~ "Missing")) %>%
                     select(col = x1poverty185, prop)

                 bind_rows(t, g, r, p) %>%
                     mutate(prop = as.character(round(prop * 100, 2))) %>%
                     rename({{ .x }} := prop)

             }) %>%
    setNames(outcomes) %>%
    reduce(left_join, by = "col")

## -----------------
## make table
## -----------------

## put together
out <- cbind(out_c_1,
             rbind(out_c_2[1:2,2],
                   "",
                   out_c_2[3:4,2],
                   "",
                   out_c_2[5:11,2],
                   "",
                   out_c_2[12:13,2],
                   ""),
             rbind(out_a[1:2,2:4],
                   "",
                   out_a[3:4,2:4],
                   "",
                   out_a[5:11,2:4],
                   "",
                   out_a[12:14,2:4])) %>%
    as.matrix

## get Ns
n <- c("",
       df_c %>% nrow,
       "",
       df_a %>% select(o_vot_reg) %>% drop_na %>% nrow,
       df_a %>% select(o_vol_ind) %>% drop_na %>% nrow,
       df_a %>% select(o_vol_log) %>% drop_na %>% nrow)

## add ns
out <- rbind(out, n)

## add italics
out[c(3,6,14,17), 1] <- to_italic(out[c(3,6,14,17), 1])

## add hspace
out[,1] <- add_hspace(out[,1], 1)

## add headers
out <- rbind(cbind("{\\bfseries Enrollment}", rbind(rep("", 5))),
             out[1:3,],
             cbind("{\\bfseries Gender}", rbind(rep("", 5))),
             out[4:6,],
             cbind("{\\bfseries Race/Ethnicity}", rbind(rep("", 5))),
             out[7:14,],
             cbind("{\\bfseries Poverty status}", rbind(rep("", 5))),
             out[15:18,])

## add name for N
out[nrow(out),1] <- "$N$"

## table note
note <- c("All values are percentages. Column (1) includes missing values in ",
          "the full HSLS sample as their own category. Column (2) recomputes the ",
          "percentages after dropping missing categorical values. Percentages ",
          "in the outcome columns represent the analytic samples used to fit each ",
          "model")

## header
header <- c("\\begin{sidewaystable}",
            "\\caption{Comparison of model samples with full HSLS sample}",
            "\\label{tab:comparison}",
            "\\begin{tabularx}{\\textwidth}{X*{2}{c}*{3}{>{\\CC}p{1.5in}}}",
            "\\toprule",
            " & \\multicolumn{2}{c}{Full HSLS} & \\multicolumn{3}{c}{Outcomes}\\\\",
            "\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}",
            "& (1) & (2) & Registered to vote & Volunteer & Log(volunteer hours)  \\\\",
            "\\midrule")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{6}{p{.98\\linewidth}}{\\footnotesize",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{tabularx}",
            "\\end{sidewaystable}")

## core content
out <- print(xtable(as.matrix(out)),
             hline.after = c(-1, nrow(out) - 1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving selection table at: ", tab_dir)
proj_message(fn,"s")
writeLines(text = c(header, out, footer),
           con = file.path(tab_dir, fn))

## -----------------------------------------------
## regression table
## -----------------------------------------------

proj_message("Making regression table")

## output file name
fn <- "regressions.tex"

## get data
df <- readRDS(file.path(est_dir, "regressions.rds"))

out <- map(outcomes,
           ~ {
               ## subset list
               tmp <- df[[.x]]

               c1 <- summary(tmp[[1]])[["coefficients"]][2,] %>% as.matrix %>% t
               c2 <- summary(tmp[[2]])[["coefficients"]][14:15,]
               c3 <- summary(tmp[[3]])[["coefficients"]][9:15,]
               c4 <- summary(tmp[[4]])[["coefficients"]][13:14,]
               c5 <- summary(tmp[[5]])[["coefficients"]][8:21,]
               c6 <- summary(tmp[[6]])[["coefficients"]][7:20,]

               ## rename cols
               colnames(c1) <- c("est", "se", "t", "pval")
               colnames(c2) <- c("est", "se", "t", "pval")
               colnames(c3) <- c("est", "se", "t", "pval")
               colnames(c4) <- c("est", "se", "t", "pval")
               colnames(c5) <- c("est", "se", "t", "pval")
               colnames(c6) <- c("est", "se", "t", "pval")

               ## set stars
               c1[,"est"] <- add_stars(round(c1[,"est"], 2), c1[,"pval"])
               c2[,"est"] <- add_stars(round(c2[,"est"], 2), c2[,"pval"])
               c3[,"est"] <- add_stars(round(c3[,"est"], 2), c3[,"pval"])
               c4[,"est"] <- add_stars(round(c4[,"est"], 2), c4[,"pval"])
               c5[,"est"] <- add_stars(round(c5[,"est"], 2), c5[,"pval"])
               c6[,"est"] <- add_stars(round(c6[,"est"], 2), c6[,"pval"])

               ## add parentheses
               c1[,"se"] <- add_paren(round(as.numeric(c1[,"se"]), 3))
               c2[,"se"] <- add_paren(round(as.numeric(c2[,"se"]), 3))
               c3[,"se"] <- add_paren(round(as.numeric(c3[,"se"]), 3))
               c4[,"se"] <- add_paren(round(as.numeric(c4[,"se"]), 3))
               c5[,"se"] <- add_paren(round(as.numeric(c5[,"se"]), 3))
               c6[,"se"] <- add_paren(round(as.numeric(c6[,"se"]), 3))

               ## make tibble
               df_out <- bind_rows(c1 %>% as_tibble(rownames = "var"),
                                   c2 %>% as_tibble(rownames = "var"),
                                   c3 %>% as_tibble(rownames = "var"),
                                   c4 %>% as_tibble(rownames = "var"),
                                   c5 %>% as_tibble(rownames = "var"),
                                   c6 %>% as_tibble(rownames = "var")) %>%
                   mutate(var = ifelse(var == "1", "t_enrolcol_ever", var)) %>%
                   select(-c(t, pval)) %>%
                   pivot_longer(c(est, se),
                                names_to = "stats",
                                values_to = .x) %>%
                   select(-stats)
           })

## bind together
out <- bind_cols(out[[1]],
                 out[[2]][,2],
                 out[[3]][,2]) %>%
    mutate(var = str_replace(var, "t_enrolcol_ever", "Enrolled"),
           var = str_replace(var, "x1sex1", "Men"),
           var = str_replace(var, "x1sex2", "Women"),
           var = str_replace(var, "x1race1", "American Indian/Alaska Native"),
           var = str_replace(var, "x1race2", "Asian"),
           var = str_replace(var, "x1race3", "Black"),
           var = str_replace(var, "x1race4", "Hispanic"),
           var = str_replace(var, "x1race6", "More than one race"),
           var = str_replace(var, "x1race7", "Native Hawaiian/Pacific Islander"),
           var = str_replace(var, "x1race8", "White"),
           var = str_replace(var, "x1poverty1850", "Above 185\\\\% of poverty line"),
           var = str_replace(var, "x1poverty1851", "Below 185\\\\% of poverty line"),
           var = str_replace_all(var, ":", " {\\\\itshape X} "),
           var = ifelse(row_number() %% 2 == 0, "", var)) %>%
    as.matrix

## add hspace
out[,1] <- add_hspace(out[,1], 1)

## add sections
out <- rbind(cbind("{\\bfseries Model 1}", rbind(rep("", 3))),
             out[1:2,],
             cbind("{\\bfseries Model 2}", rbind(rep("", 3))),
             out[3:6,],
             cbind("{\\bfseries Model 3}", rbind(rep("", 3))),
             out[7:20,],
             cbind("{\\bfseries Model 4}", rbind(rep("", 3))),
             out[21:24,],
             cbind("{\\bfseries Model 5}", rbind(rep("", 3))),
             out[25:52,],
             cbind("{\\bfseries Model 6}", rbind(rep("", 3))),
             out[53:80,])

note <- c("$^{***} p < 0.001$; $^{**} p < 0.01$; $^{*} p < 0.05$. ",
          "Primary point estimates from linear probability models (LPM) and ",
          "ordinary least squares (OLS) regressions are shown, with ",
          "standard errors in parentheses. All models include indicators for ",
          "gender, race/ethnicity, and poverty status (under 185\\% federal ",
          "poverty line) as well as controls ",
          "for base year socioeconomic status and region.")

## header
header <- c("\\begin{longtable}{>{\\RR}p{3.5in}ccc}",
            paste0("\\caption{Results from outcomes regressed on ",
                   "indicator for college participation} \\\\"),
            "\\label{tab:regressions} \\\\",
            "\\toprule",
            "& Registered to vote & Volunteer & Log(volunteer hours) \\\\",
            "\\midrule",
            "\\endfirsthead",
            "\\multicolumn{4}{l}{\\emph{...table \\thetable{} continued}} \\\\",
            "\\toprule",
            "& Registered to vote & Volunteer & Log(volunteer hours) \\\\",
            "\\midrule",
            "\\endhead",
            "\\bottomrule",
            "\\multicolumn{4}{r}{\\emph{Continued on next page...}} \\\\",
            "\\endfoot",
            "\\endlastfoot")

## footer
footer <- c("\\bottomrule",
            "\\multicolumn{4}{p{.98\\linewidth}}{\\footnotesize",
            "{\\bfseries Note.} ",
            note,
            "}",
            "\\end{longtable}")

## core content
out <- print(xtable(out),
             hline.after = c(-1),
             sanitize.text.function = function(x) {x},
             booktabs = TRUE,
             tabular.environment = "longtable",
             floating = FALSE,
             caption.placement = "top",
             include.rownames = FALSE,
             include.colnames = FALSE,
             timestamp = NULL,
             print.results = FALSE,
             only.contents = TRUE)

proj_message("Saving selection table at: ", tab_dir)
proj_message(fn,"s")
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
