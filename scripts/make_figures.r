################################################################################
##
## [ PROJ ] Postsecondary civic outcomes
## [ FILE ] make_figures.r
## [ AUTH ] Benjamin Skinner & Will Doyle
## [ INIT ] 11 July 2019
##
################################################################################

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, "..", args)
data_dir <- file.path(root, "data")
cln_dir <- file.path(data_dir, "clean")
est_dir <- file.path(root, "estimates")
scr_dir <- file.path(root, "scripts")
des_dir <- file.path(root, "descriptives")
fig_dir <- file.path(root, "figures")

## source utils
source(file.path(scr_dir, "utils.r"))

## libraries
quiet_require(c("tidyverse","grf","ggrepel","ggpubr","extrafont","gridExtra","patchwork"))

## header message
proj_message("make_figures.r","h")

## -----------------------------------------------
## plot functions
## -----------------------------------------------

pt_to_mm <- function(x) x / 2.83465

to_plot_names <- function(x) {
    case_when(
        x == "m" ~ "Men",
        x == "f" ~ "Women",
        x == "men" ~ "Men",
        x == "women" ~ "Women",
        x == "ameri" ~ "American Indian/Alaska Native",
        x == "asian" ~ "Asian",
        x == "black" ~ "Black",
        x == "hispc" ~ "Hispanic",
        x == "multp" ~ "More than one race",
        x == "natpi" ~ "Native Hawaiian/Pacific Islander",
        x == "white" ~ "White",
        x == "pov185b" ~ "Below 185% of poverty line",
        x == "pov185a" ~ "Above 185% of poverty line",
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
        x == "o_vot_reg" ~ "Registered to vote"
    )
}

bar_plot_desc <- function(df,
                          outcome,
                          width = 7.5,
                          height = 5.25,
                          text_size = 11) {

    ## filter data to outcome
    plot_df <- filter(df, outcome == {{ outcome }})

    ## set x-axis labels depending on outcome
    x_labs <- case_when(
        str_detect({{ outcome }}, "vot") ~ c("Not registered",
                                             "Registered"),
        str_detect({{ outcome }}, "ind") ~ c("Did not volunteer",
                                             "Volunteered")
    )

    ## make plot
    ggplot(plot_df,
           aes(x = factor(o,
                          levels = 0:1,
                          labels = x_labs),
               y = y,
               fill = factor(t,
                             levels = c(0,1),
                             labels = c("Not enrolled", "Enrolled")),
               linetype = factor(t,
                                 levels = c(0,1),
                                 labels = c("Not enrolled","Enrolled")),
               label = paste0(round(y, 1),"%"))) +
        geom_bar(stat = "identity", position = "dodge2", alpha = 0.4, colour = "black",
                 show.legend = FALSE) +
        scale_linetype_manual(values = c("solid","dashed")) +
        geom_text(size = pt_to_mm(text_size),
                  family = "Times New Roman",
                  lineheight = .8,
                  position = position_dodge(width = .9), vjust = -0.3) +
        ylim(c(0,100)) +
        labs(title = to_plot_names({{ outcome }}),
             x = NULL,
             y = "Percentage") +
        theme_bw(base_size = text_size) +
        theme(text = element_text(family = "Times New Roman", size = text_size),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(size = text_size),
              axis.title.y = element_text(size = text_size),
              panel.grid.major.x = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = text_size),
              strip.text.x = element_text(size = text_size),
              strip.background = element_rect(fill = "white"))
}

hist_desc <- function(df,
                      outcome,
                      width = 7.5,
                      height = 5.25,
                      text_size = 11) {

    ## filter data to outcome
    plot_df <- df %>%
        select(t, x = {{ outcome }}) %>%
        drop_na

    ## set x-axis labels depending on outcome
    x_label <- case_when(
        str_detect({{ outcome }}, "hrs") ~ "Hours",
        str_detect({{ outcome }}, "pos") ~ "Hours",
        str_detect({{ outcome }}, "log") ~ "Log hours",
    )

    ## make plot
    ggplot(plot_df, aes(x = x)) +
        geom_density(aes(y = ..scaled..,
                         fill = factor(t,
                                       levels = c(0,1),
                                       labels = c("Not enrolled","Enrolled")),
                         linetype = factor(t,
                                           levels = c(0,1),
                                           labels = c("Not enrolled","Enrolled"))),
                     alpha = 0.4,
                     position = "identity",
                     adjust = 3) +
        scale_linetype_manual(values = c("solid","dashed")) +
        labs(title = to_plot_names({{ outcome }}),
             x = x_label,
             y = "Density") +
        theme_bw(base_size = text_size) +
        theme(text = element_text(family = "Times New Roman", size = text_size),
              ## axis.title.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.y = element_text(size = text_size),
              panel.grid.minor = element_blank(),
              panel.grid.major.y = element_blank(),
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size = text_size),
              strip.text.x = element_text(size = text_size),
              strip.background = element_rect(fill = "white"))
}

hist_tau <- function(spec,
                     width = 7.5,
                     height = 5.25,
                     theme_base_size = 12,
                     text_size = 11) {

    ## set figure name using specification ending
    ## fn <- paste0("tau_hist_", spec)

    ## read causal forest objects into memory
    cf_vot_file <- file.path(est_dir, paste0("o_vot_reg_", spec, ".rds"))
    cf_vol_file <- file.path(est_dir, paste0("o_vol_ind_", spec, ".rds"))
    cf_vll_file <- file.path(est_dir, paste0("o_vol_log_", spec, ".rds"))
    cf_vot <- readRDS(cf_vot_file)
    cf_vol <- readRDS(cf_vol_file)
    cf_vll <- readRDS(cf_vll_file)

    ## get predictions from causal forest object
    preds_vot <- predict(cf_vot)[["predictions"]]
    preds_vol <- predict(cf_vol)[["predictions"]]
    preds_vll <- predict(cf_vll)[["predictions"]]

    ## make data for plotting
    plot_df <- bind_rows(tibble(outcome = "o_vot_reg",
                                preds = preds_vot),
                         tibble(outcome = "o_vol_ind",
                                preds = preds_vol),
                         tibble(outcome = "o_vol_log",
                                preds = preds_vll))

    ## max/min bounds across all three, with some padding
    max_x <- plot_df %>% pull(preds) %>% max %>% round_any(0.05, ceiling)
    min_x <- plot_df %>% pull(preds) %>% min %>% round_any(0.05, floor)

    outcome_vec <- c("o_vot_reg", "o_vol_ind", "o_vol_log")

    ## plot
    plot_list <- list()
    for (i in 1:3) {

        plot_list[[i]] <- ggplot(plot_df %>% filter(outcome == outcome_vec[i]),
                                 aes(x = preds)) +
            geom_histogram(binwidth = 0.01, colour = "black",
                           fill = "#7C9B99", alpha = 0.4) +
            geom_vline(xintercept = 0, linetype = "dashed") +
            labs(y = "Frequency",
                 x = "Estimated \"out-of-bag\" \u03C4(x)", # \u03C4 := \tau
                 title = NULL) +
            scale_x_continuous(breaks = seq(min_x, max_x, 0.1)) +
            coord_cartesian(xlim = c(min_x, max_x)) +
            theme_bw(base_size = theme_base_size) +
            theme(text = element_text(family = "Times New Roman",
                                      size = text_size),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(size = text_size),
                  strip.background = element_rect(fill = "white"),
                  legend.text = element_text(size = text_size),
                  legend.title = element_blank())
    }
    plot_list
}

coef_plot_1 <- function(df, outcome_1, outcome_2, outcome_3, subgroups,
                      facet_var, fn,
                      width = 14, height = 9, theme_base_size = 12,
                      text_size = 11) {

    ## get plot data frames into list
    plot_df_list <- map(c(outcome_1, outcome_2, outcome_3),
                        ~ df %>%
                            filter(outcome == .x,
                                   group %in% subgroups))

    ## want same y-axis scale for combo plots, so need to get abs(max(95CI))
    ## between both and set here
    max_ylim_1 <- max(plot_df_list[[1]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[1]] %>% pull(lo95) %>% min))
    max_ylim_2 <- max(plot_df_list[[2]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[2]] %>% pull(lo95) %>% min))
    abs_max <- max(1, max_ylim_1, max_ylim_2)
    ## x_nudge for labels
    nudge_x_pos <- 0
    nudge_x_neg <- 0
    gg <- list()
    for (i in 1:length(plot_df_list)) {
        plot_df <- plot_df_list[[i]]
        if (unique(grepl("vol_log", plot_df$outcome))) {
            abs_max <- max(plot_df %>% pull(hi95) %>% max,
                           abs(plot_df %>% pull(lo95) %>% min))
        }
        ylimits <- c((abs_max * -1) - (abs_max * .05),
                     abs_max + (abs_max * .05))
        ptitle <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ to_plot_names("o_vot_reg"),
            grepl("vol_ind", plot_df$outcome) ~ to_plot_names("o_vol_ind"),
            grepl("vol_log", plot_df$outcome) ~ to_plot_names("o_vol_log")
        ) %>% unique
        ## y label
        ylab <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_ind", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_log", plot_df$outcome) ~ "Percentage change"
        ) %>% unique
        ## set up plot data
        plot_df <- plot_df %>%
            mutate(x_run = c(3,5,1:7,3,5),
                   group = factor(group,
                                  levels = group,
                                  labels = to_plot_names(group)))

        plot_dat <- split(plot_df, f = plot_df[[facet_var]])
        plot_dat["Propensity"] <- NULL
        plot_dat["Overall"] <- NULL
        ff <- list()
        for (j in 1:length(plot_dat)) {
            ## get observation data frame (prevent overplotting of label)
            plot_dat_obs <- plot_dat[[j]] %>%
                group_by(group) %>%
                filter(row_number() == 1)
            ## plot
            ff[[j]] <- ggplot(plot_dat[[j]],
                              aes(x = x_run,
                                  y = est,
                                  colour = group,
                                  shape = group,
                                  label = paste0(round(est, 2), "\n(",
                                                 pval_func(est,se), ")"))) +
                geom_hline(yintercept = 0, linetype = "dashed") +
                geom_linerange(aes(ymin = lo95, ymax = hi95), size = 0.5) +
                facet_wrap(as.formula(paste("~", facet_var))) +
                geom_point(fill = "white", size = pt_to_mm(text_size - 1)) +
                geom_point(size = pt_to_mm(text_size - 1)) +
                {
                    sub_plot_df <- subset(plot_dat[[j]], est > 0)
                    if (nrow(sub_plot_df) > 0) {
                        geom_text_repel(data = sub_plot_df,
                                        ylim = c(0,NA),
                                        ## use full df for cases when all sub_ are -/+
                                        xlim = c(min(plot_dat[[j]]$x_run),
                                                 max(plot_dat[[j]]$x_run)),
                                        nudge_x = nudge_x_pos,
                                        colour = "black",
                                        point.padding = 0.5,
                                        segment.color = "grey50",
                                        segment.alpha = 1,
                                        segment.size = .3,
                                        size = pt_to_mm(text_size - 1),
                                        family = "Times New Roman",
                                        force = 10,
                                        lineheight = .8)
                    }
                } +
                {
                    sub_plot_df <- subset(plot_dat[[j]], est < 0)
                    if (nrow(sub_plot_df) > 0) {
                        geom_text_repel(data = sub_plot_df,
                                        ylim = c(NA,0),
                                        ## use full df for cases when all sub_ are -/+
                                        xlim = c(min(plot_dat[[j]]$x_run),
                                                 max(plot_dat[[j]]$x_run)),
                                        nudge_x = nudge_x_neg,
                                        colour = "black",
                                        point.padding = 0.5,
                                        segment.color = "grey50",
                                        segment.alpha = 1,
                                        segment.size = .3,
                                        size = pt_to_mm(text_size - 1),
                                        family = "Times New Roman",
                                        force = 10,
                                        lineheight = .8)
                    }
                } +
                scale_y_continuous(breaks = scales::pretty_breaks(),
                                   limits = ylimits) +
                scale_x_discrete(breaks = 1:7) +
                scale_shape_manual(values = c("Men" = 15,
                                              "Women" = 16,
                                              "American Indian/Alaska Native" = 15,
                                              "Asian" = 16,
                                              "Black" = 18,
                                              "Hispanic" = 21,
                                              "More than one race" = 22,
                                              "Native Hawaiian/Pacific Islander" = 23,
                                              "White" = 24,
                                              "Below 185% of poverty line" = 25,
                                              "Above 185% of poverty line" = 24)) +
                coord_cartesian(clip = "off") +
                geom_text(aes(label = formatC(obs, format = "d", big.mark = ",")),
                          y = min(ylimits) - .2 * abs(min(ylimits)),
                          colour = "black",
                          family = "Times New Roman",
                          size = pt_to_mm(text_size - 1)) +
                guides(colour = NULL,
                       shape = guide_legend(ncol = 2, byrow = TRUE)) +
                theme_bw(base_size = theme_base_size) +
                theme(text = element_text(family = "Times New Roman",
                                          size = text_size),
                      axis.title.x = element_blank(),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_rect(size = 0.25),
                      plot.margin = unit(c(1,0.25,1,0.25), "lines"),
                      strip.text.x = element_text(size = text_size),
                      strip.background = element_rect(fill = "white"),
                      legend.text = element_text(size = text_size),
                      legend.title = element_blank())
            if (j == 1) {
                ## leftmost plot
                ff[[j]] <- ff[[j]] +
                    labs(y = ylab,
                         x = NULL,
                         title = ptitle)

            } else {
                ## other two plots
                ff[[j]] <- ff[[j]] +
                    labs(y = NULL,
                         x = NULL,
                         title = " ") +
                    theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank())
            }
            if (i == 3) {
                ff[[j]] <- ff[[j]] +
                    theme(legend.position = "bottom")
            } else {
                ff[[j]] <- ff[[j]] +
                    theme(legend.position = "none")
            }

        }
        gg[[i]] <- ff[[1]] | ff[[2]] | ff[[3]]
    }
    ## combine plots
    g <- gg[[1]] / gg[[2]] / gg[[3]]

    ## message
    proj_message(paste0(fn, ".pdf"),"s")
    ## save
    ggsave(filename = paste0(fn, ".pdf"),
           plot = g,
           device = cairo_pdf,
           path = fig_dir,
           width = width,
           height = height,
           units = "in",
           dpi = "retina")
}

coef_plot_2 <- function(df, outcome_1, outcome_2, outcome_3, subgroups, fn,
                        facet_var = "facet_var", switch_facet = FALSE,
                        width = 12, height = 9, theme_base_size = 12,
                        text_size = 11) {

    ## which is facet and which is factor
    split <- if(switch_facet) {
                 c(facet_var, "group")
             } else {
                 c("group", facet_var)
             }
    ## get plot data frames into list
    plot_df_list <- map(c(outcome_1, outcome_2, outcome_3),
                        ~ df %>%
                            filter(outcome == .x,
                                   group %in% subgroups) %>%
                            separate(group, split, sep = "_"))

    ## want same y-axis scale for combo plots, so need to get abs(max(95CI))
    ## between both and set here
    max_ylim_1 <- max(plot_df_list[[1]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[1]] %>% pull(lo95) %>% min))
    abs_max <- max(1, max_ylim_1)
    if (!isFALSE(outcome_2)) {
        max_ylim_2 <- max(plot_df_list[[2]] %>% pull(hi95) %>% max,
                          abs(plot_df_list[[2]] %>% pull(lo95) %>% min))
        abs_max <- max(1, max_ylim_1, max_ylim_2)
    }
    yseq_unit <- case_when(
        grepl("vv", fn) ~ 0.5
    )
    ## x_nudge for labels
    if (!isFALSE(outcome_2)) {
        nudge_x_pos = -0.01
        nudge_x_neg = -0.03
    } else {
        nudge_x_pos = -0.02
        nudge_x_neg = -0.05
    }
    gg <- list()
    for (i in 1:length(plot_df_list)) {
        plot_df <- plot_df_list[[i]]
        if (unique(grepl("vol_log", plot_df$outcome))) {
            abs_max <- max(plot_df %>% pull(hi95) %>% max,
                           abs(plot_df %>% pull(lo95) %>% min))
        }
        ylimits <- c((abs_max * -1) - (abs_max * .05),
                     abs_max + (abs_max * .05))
        ptitle <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ to_plot_names("o_vot_reg"),
            grepl("vol_ind", plot_df$outcome) ~ to_plot_names("o_vol_ind"),
            grepl("vol_log", plot_df$outcome) ~ to_plot_names("o_vol_log")
        ) %>% unique
        ## y label
        ylab <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_ind", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_log", plot_df$outcome) ~ "Percentage change"
        ) %>% unique
        ## set up plot data
        plot_df <- group_by(plot_df,
                            !!sym(facet_var)) %>%
            mutate(x_run = row_number(),
                   panel_n = sum(obs),
                   group = factor(group,
                                  levels = group,
                                  labels = to_plot_names(group))) %>%
            ungroup %>%
            mutate(facet_var = to_plot_names(facet_var))

        ## plot
        gg[[i]] <- ggplot(plot_df, aes(x = x_run,
                                       y = est,
                                       colour = group,
                                       shape = group,
                                       label = paste0(round(est, 2), "\n(",
                                                      pval_func(est,se), ")"))) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_linerange(aes(ymin = lo95, ymax = hi95), size = 0.5) +
            facet_wrap(as.formula(paste("~", facet_var))) +
            geom_point(fill = "white", size = pt_to_mm(text_size - 1)) +
            geom_point(size = pt_to_mm(text_size - 1)) +
            {
                sub_plot_df <- subset(plot_df, est > 0)
                if (nrow(sub_plot_df) > 0) {
                    geom_text_repel(data = sub_plot_df,
                                    ylim = c(0,NA),
                                    ## use full df for cases when all sub_ are -/+
                                    xlim = c(min(plot_df$x_run),
                                             max(plot_df$x_run)),
                                    nudge_x = nudge_x_pos,
                                    colour = "black",
                                    point.padding = 0.5,
                                    segment.color = "grey50",
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(text_size - 1),
                                    family = "Times New Roman",
                                    force = 10,
                                    lineheight = .8)
                }
            } +
            {
                sub_plot_df <- subset(plot_df, est < 0)
                if (nrow(sub_plot_df) > 0) {
                    geom_text_repel(data = sub_plot_df,
                                    ylim = c(NA,0),
                                    ## use full df for cases when all sub_ are -/+
                                    xlim = c(min(plot_df$x_run),
                                             max(plot_df$x_run)),
                                    nudge_x = nudge_x_neg,
                                    colour = "black",
                                    point.padding = 0.5,
                                    segment.color = "grey50",
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(text_size - 1),
                                    family = "Times New Roman",
                                    force = 10,
                                    lineheight = .8)
                }
            } +
            scale_y_continuous(breaks = scales::pretty_breaks(),
                               limits = ylimits) +
            scale_shape_manual(values = c("Men" = 15,
                                          "Women" = 16,
                                          "American Indian/Alaska Native" = 15,
                                          "Asian" = 16,
                                          "Black" = 18,
                                          "Hispanic" = 21,
                                          "More than one race" = 22,
                                          "Native Hawaiian/Pacific Islander" = 23,
                                          "White" = 24,
                                          "Below 185% of poverty line" = 25,
                                          "Above 185% of poverty line" = 24)) +
            coord_cartesian(clip = "off") +
            geom_text(aes(label = formatC(obs, format = "d", big.mark = ",")),
                      y = min(ylimits) - .2 * abs(min(ylimits)),
                      colour = "black",
                      family = "Times New Roman",
                      size = pt_to_mm(text_size - 1)) +
            labs(y = ylab,
                 x = NULL,
                 title = ptitle) +
            theme_bw(base_size = theme_base_size) +
            guides(colour = NULL,
                   shape = guide_legend(ncol = 3, byrow = TRUE)) +
            theme(text = element_text(family = "Times New Roman",
                                      size = text_size),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(size = 0.25),
                  plot.margin = unit(c(1,1,1,1), "lines"),
                  strip.text.x = element_text(size = text_size),
                  strip.background = element_rect(fill = "white"),
                  legend.text = element_text(size = text_size),
                  legend.title = element_blank())
    }
    ## combine plots
    g <- ggpubr::ggarrange(gg[[1]],
                           gg[[2]],
                           gg[[3]],
                           ncol = 1,
                           common.legend = TRUE,
                           legend = "bottom")

    ## message
    proj_message(paste0(fn, ".pdf"),"s")
    ## save
    ggsave(filename = paste0(fn, ".pdf"),
           plot = g,
           device = cairo_pdf,
           path = fig_dir,
           width = width,
           height = height,
           units = "in",
           dpi = "retina")
}

coef_plot_3 <- function(df, outcome_1, outcome_2, outcome_3, subgroups,
                        fn, width = 14, height = 9, theme_base_size = 12,
                        text_size = 11) {

    ## get plot data frames into list
    plot_df_list <- map(c(outcome_1, outcome_2, outcome_3),
                        ~ df %>%
                            filter(outcome == .x,
                                   group %in% subgroups))

    ## want same y-axis scale for combo plots, so need to get abs(max(95CI))
    ## between both and set here
    max_ylim_1 <- max(plot_df_list[[1]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[1]] %>% pull(lo95) %>% min))
    max_ylim_2 <- max(plot_df_list[[2]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[2]] %>% pull(lo95) %>% min))
    abs_max <- max(1, max_ylim_1, max_ylim_2)
    ## x_nudge for labels
    nudge_x_pos <- 0
    nudge_x_neg <- 0
    gg <- list()
    for (i in 1:length(plot_df_list)) {
        plot_df <- plot_df_list[[i]]
        if (unique(grepl("vol_log", plot_df$outcome))) {
            abs_max <- max(plot_df %>% pull(hi95) %>% max,
                           abs(plot_df %>% pull(lo95) %>% min))
        }
        ylimits <- c((abs_max * -1) - (abs_max * .05),
                     abs_max + (abs_max * .05))
        ptitle <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ to_plot_names("o_vot_reg"),
            grepl("vol_ind", plot_df$outcome) ~ to_plot_names("o_vol_ind"),
            grepl("vol_log", plot_df$outcome) ~ to_plot_names("o_vol_log")
        ) %>% unique
        ## y label
        ylab <- case_when(
            grepl("vot_reg", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_ind", plot_df$outcome) ~ "Percentage point change",
            grepl("vol_log", plot_df$outcome) ~ "Percentage change"
        ) %>% unique
        ## set up plot data
        plot_df <- plot_df %>%
            mutate(x_run = 1:10,
                   group = factor(group,
                                  levels = group,
                                  labels = to_plot_names(group)))
        ## plot
        gg[[i]] <- ggplot(plot_df,
                          aes(x = x_run,
                              y = est,
                              colour = group,
                              shape = group,
                              label = paste0(round(est, 2), "\n(",
                                             pval_func(est,se), ")"))) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            geom_linerange(aes(ymin = lo95, ymax = hi95), size = 0.5) +
            geom_point(fill = "white", size = pt_to_mm(text_size - 1)) +
            geom_point(size = pt_to_mm(text_size - 1)) +
            {
                sub_plot_df <- subset(plot_df, est > 0)
                if (nrow(sub_plot_df) > 0) {
                    geom_text_repel(data = sub_plot_df,
                                    ylim = c(0,NA),
                                    ## use full df for cases when all sub_ are -/+
                                    xlim = c(min(plot_df$x_run),
                                                 max(plot_df$x_run)),
                                    nudge_x = nudge_x_pos,
                                    colour = "black",
                                    point.padding = 0.5,
                                    segment.color = "grey50",
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(text_size - 1),
                                    family = "Times New Roman",
                                    force = 10,
                                    lineheight = .8)
                }
            } +
            {
                sub_plot_df <- subset(plot_df, est < 0)
                if (nrow(sub_plot_df) > 0) {
                    geom_text_repel(data = sub_plot_df,
                                    ylim = c(NA,0),
                                    ## use full df for cases when all sub_ are -/+
                                    xlim = c(min(plot_df$x_run),
                                             max(plot_df$x_run)),
                                    nudge_x = nudge_x_neg,
                                    colour = "black",
                                    point.padding = 0.5,
                                    segment.color = "grey50",
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(text_size - 1),
                                    family = "Times New Roman",
                                    force = 10,
                                    lineheight = .8)
                }
            } +
            scale_y_continuous(breaks = scales::pretty_breaks(),
                               limits = ylimits) +
            scale_x_discrete(breaks = 1:10) +
            scale_shape_manual(values = c("(0,0.5]" = 15,
                                          "(0.5,0.6]" = 22,
                                          "(0.6,0.7]" = 16,
                                          "(0.7,0.8]" = 21,
                                          "(0.8,0.85]" = 17,
                                          "(0.85,0.9]" = 24,
                                          "(0.9,0.925]" = 18,
                                          "(0.925,0.95]" = 23,
                                          "(0.95,0.975]" = 8,
                                          "(0.975,1]" = 25)) +
            coord_cartesian(clip = "off") +
            geom_text(aes(label = formatC(obs, format = "d", big.mark = ",")),
                      y = min(ylimits) - .2 * abs(min(ylimits)),
                      colour = "black",
                      family = "Times New Roman",
                      size = pt_to_mm(text_size - 1)) +
            guides(colour = NULL,
                   shape = guide_legend(row = 2, byrow = FALSE)) +
            labs(y = ylab,
                 x = NULL,
                 title = ptitle) +
            theme_bw(base_size = theme_base_size) +
            theme(text = element_text(family = "Times New Roman",
                                      size = text_size),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_rect(size = 0.25),
                  plot.margin = unit(c(1,0.25,1,0.25), "lines"),
                  strip.text.x = element_text(size = text_size),
                  strip.background = element_rect(fill = "white"),
                  legend.text = element_text(size = text_size),
                  legend.title = element_blank())
        if (i == 3) {
            gg[[i]] <- gg[[i]] +
                theme(legend.position = "bottom")
        } else {
            gg[[i]] <- gg[[i]] +
                theme(legend.position = "none")
        }

    }
    ## combine plots
    g <- gg[[1]] / gg[[2]] / gg[[3]]

    ## message
    proj_message(paste0(fn, ".pdf"),"s")
    ## save
    ggsave(filename = paste0(fn, ".pdf"),
           plot = g,
           device = cairo_pdf,
           path = fig_dir,
           width = width,
           height = height,
           units = "in",
           dpi = "retina")
}

## -----------------------------------------------
## plot: descriptive bar plots
## -----------------------------------------------

proj_message("Reading discrete descriptive stats into memory")
df_d <- readRDS(file.path(des_dir, "desc_discrete.rds")) %>%
    filter(outcome %in% c("o_vot_reg","o_vol_ind")) %>%
    select(outcome, o = value, t = t_enrolcol_ever, n, y = pct_within_treat)

## make plots
bp <- map(c("o_vot_reg", "o_vol_ind"),
          ~ bar_plot_desc(df_d, .x))

## -----------------------------------------------
## plot: descriptive histogram
## -----------------------------------------------

df_c <- readRDS(file.path(cln_dir, "analysis.rds")) %>%
    select(starts_with("o_vol"), -ends_with("_ind"), t = t_enrolcol_ever)

## make plots
hp <- map(c("o_vol_hrs", "o_vol_pos", "o_vol_log"),
          ~ hist_desc(df_c, .x))

## -----------------------------------------------
## plot: histogram of OOB tau
## -----------------------------------------------

proj_message("Histogram of OOB tau")

## list of files
files <- list.files(file.path(est_dir), "*.rds", full.names = TRUE)

## file endings for various specifications
spec_vec <- str_replace(get_basename(files), ".+_(cf.*$)", "\\1") %>% unique

## make plots
tau <- map(spec_vec,
           ~ hist_tau(.)) %>%
    setNames(spec_vec)

## -----------------------------------------------
## plot: combine observed with tau hist
## -----------------------------------------------

## observed: vertical LHS
g_l <- bp[[1]] / bp[[2]] / hp[[3]]

## tau: vertical RHS
g_r <- tau[["cf_vi_q80"]][[1]] / tau[["cf_vi_q80"]][[2]] / tau[["cf_vi_q80"]][[3]]

## combine LHS + RHS
g <- ( g_l | g_r )

## adjust legend on bottom
g <- g + plot_layout(guides = "collect") &
    theme(legend.position = "bottom",
          legend.justification = c(.2,0))

## save
ggsave(filename = "desc_het.pdf",
       plot = g,
       device = cairo_pdf,
       path = fig_dir,
       width = 12,
       height = 9,
       units = "in",
       dpi = "retina")

## -----------------------------------------------
## read in analysis estimates
## -----------------------------------------------

proj_message("Reading estimates into memory")
files <- list.files(file.path(est_dir), "*_est.csv", full.names = TRUE)
fname <- get_basename(files) %>% str_replace("_est", "")
df <- map(files,
          ~ read_csv(.x,
                     col_types = cols(group = "c",
                                      outcome = "c",
                                      .default = "d"))) %>%
    bind_rows %>%
    mutate(group_cat = case_when(
               str_detect(group, "overall") ~ 0,
               str_detect(group, "men|women") ~ 1,
               str_detect(group,
                          "ameri|asian|black|hispc|multp|natpi|white") ~ 2,
               str_detect(group, "pov185a|pov185b") ~ 3,
               TRUE ~ 4
           ),
           group_cat = factor(group_cat,
                              levels = 0:4,
                              labels = c("Overall",
                                         "Gender",
                                         "Race/ethnicity",
                                         "185% Poverty Line",
                                         "Propensity")))

proj_message("Saving plots at: ", fig_dir)

## -----------------------------------------------
## plot: gender X race/ethnicity X poverty status
## -----------------------------------------------

subgroups <- grep("_|non|overall", distinct(df, group) %>% pull,
                  value = TRUE, invert = TRUE)

## vote/volunteering
walk(c("pos","q50","q80","q90","q95"),
     ~ {
         spec <- .x
         top_f <- paste0("o_vot_reg_cf_vi_", spec)
         mid_f <- paste0("o_vol_ind_cf_vi_", spec)
         bot_f <- paste0("o_vol_log_cf_vi_", spec)
         out_f <- paste0("gender_race_pov_", spec)
         coef_plot_1(df, top_f, mid_f, bot_f, subgroups, "group_cat", out_f)
         top_f <- paste0("o_vot_reg_cfw_vi_", spec)
         mid_f <- paste0("o_vol_ind_cfw_vi_", spec)
         bot_f <- paste0("o_vol_log_cfw_vi_", spec)
         out_f <- paste0("gender_race_pov_w_", spec)
         coef_plot_1(df, top_f, mid_f, bot_f, subgroups, "group_cat", out_f)
     })

## -----------------------------------------------
## plot: gender X race
## -----------------------------------------------

subgroups <- grep("*_f$|*_m$", distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep("pov|non", subgroups, value = TRUE, invert = TRUE)

## vote/volunteering
walk(c("pos","q50","q80","q90","q95"),
     ~ {
         top_f <- paste0("o_vot_reg_cf_vi_", .x)
         mid_f <- paste0("o_vol_ind_cf_vi_", .x)
         bot_f <- paste0("o_vol_log_cf_vi_", .x)
         out_f <- paste0("gender_x_race_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f)
         top_f <- paste0("o_vot_reg_cfw_vi_", .x)
         mid_f <- paste0("o_vol_ind_cfw_vi_", .x)
         bot_f <- paste0("o_vol_log_cfw_vi_", .x)
         out_f <- paste0("gender_x_race_w_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f)
     })

## -----------------------------------------------
## plot: poverty X race
## -----------------------------------------------

subgroups <- grep("^pov", distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep("_m_|_f_|_m$|_f$|non", subgroups, value = TRUE, invert = TRUE)
subgroups <- grep("_", subgroups, value = TRUE, invert = FALSE)

## vote/volunteering
walk(c("pos","q50","q80","q90","q95"),
     ~ {
         top_f <- paste0("o_vot_reg_cf_vi_", .x)
         mid_f <- paste0("o_vol_ind_cf_vi_", .x)
         bot_f <- paste0("o_vol_log_cf_vi_", .x)
         out_f <- paste0("poverty_x_race_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f, switch_facet = TRUE)
         top_f <- paste0("o_vot_reg_cfw_vi_", .x)
         mid_f <- paste0("o_vol_ind_cfw_vi_", .x)
         bot_f <- paste0("o_vol_log_cfw_vi_", .x)
         out_f <- paste0("poverty_x_race_w_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f, switch_facet = TRUE)
     })

## -----------------------------------------------
## plot: poverty X gender
## -----------------------------------------------

subgroups <- grep("^pov185[ab]_[mf]$", distinct(df, group) %>% pull, value = TRUE)

## vote/volunteering
walk(c("pos","q50","q80","q90","q95"),
     ~ {
         top_f <- paste0("o_vot_reg_cf_vi_", .x)
         mid_f <- paste0("o_vol_ind_cf_vi_", .x)
         bot_f <- paste0("o_vol_log_cf_vi_", .x)
         out_f <- paste0("poverty_x_gender_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f, switch_facet = TRUE)
         top_f <- paste0("o_vot_reg_cfw_vi_", .x)
         mid_f <- paste0("o_vol_ind_cfw_vi_", .x)
         bot_f <- paste0("o_vol_log_cfw_vi_", .x)
         out_f <- paste0("poverty_x_gender_w_", .x)
         coef_plot_2(df, top_f, mid_f, bot_f, subgroups, out_f, switch_facet = TRUE)
     })

## -----------------------------------------------
## plot: propensities
## -----------------------------------------------

subgroups <- grep("^cut_", distinct(df, group) %>% pull, value = TRUE)

## vote/volunteering
walk(c("pos","q50","q80","q90","q95"),
     ~ {
         top_f <- paste0("o_vot_reg_cf_vi_", .x)
         mid_f <- paste0("o_vol_ind_cf_vi_", .x)
         bot_f <- paste0("o_vol_log_cf_vi_", .x)
         out_f <- paste0("propensity_", .x)
         coef_plot_3(df, top_f, mid_f, bot_f, subgroups, out_f)
         top_f <- paste0("o_vot_reg_cfw_vi_", .x)
         mid_f <- paste0("o_vol_ind_cfw_vi_", .x)
         bot_f <- paste0("o_vol_log_cfw_vi_", .x)
         out_f <- paste0("propensity_w_", .x)
         coef_plot_3(df, top_f, mid_f, bot_f, subgroups, out_f)
     })

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
