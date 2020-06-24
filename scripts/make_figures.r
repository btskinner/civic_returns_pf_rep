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
root <- ifelse(length(args) == 0, '..', args)
est_dir <- file.path(root, 'estimates')
scr_dir <- file.path(root, 'scripts')
des_dir <- file.path(root, 'descriptives')
fig_dir <- file.path(root, 'figures')

## source utils
source(file.path(scr_dir, 'utils.r'))

## libraries
quiet_require(c('tidyverse','grf','ggrepel','ggpubr','extrafont'))

## header message
proj_message('make_figures.r','h')

## -----------------------------------------------
## plot functions
## -----------------------------------------------

pt_to_mm <- function(x) x / 2.83465

to_plot_names <- function(x) {
    case_when(
        x == 'm' ~ 'Men',
        x == 'f' ~ 'Women',
        x == 'ameri' ~ 'American Indian/Alaska Native',
        x == 'asian' ~ 'Asian',
        x == 'black' ~ 'Black',
        x == 'hispc' ~ 'Hispanic',
        x == 'multp' ~ 'More than one race',
        x == 'natpi' ~ 'Native Hawaiian/Pacific Islander',
        x == 'white' ~ 'White',
        x == 'pov185b' ~ 'Below 185% of poverty line',
        x == 'pov185a' ~ 'Above 185% of poverty line',
        x == 'vol_ind' ~ 'Volunteered (2015)',
        x == 'vot_reg' ~ 'Registered to vote (2016)',
        x == 'job_bal' ~ 'Importance of salary relative to work/life balance',
        x == 'job_con' ~ 'Importance of salary relative to societal contribution'
    )
}

bar_plot_disc <- function(df, w_type, width = 7.5, height = 5) {
    plot_order <- c('vot_reg','vol_ind','job_bal','job_con')
    subtitle <- case_when(
        w_type == 'unweighted' ~ NA_character_,
        w_type == 'weighted' ~ '(Weighted)')
    fn_end <- case_when(
        w_type == 'unweighted' ~ 'uw',
        w_type == 'weighted' ~ 'w')
    subdir <- case_when(
        w_type == 'unweighted' ~ 'unweighted',
        w_type == 'weighted' ~ 'weighted')
    fn <- paste0('descriptive_discrete_', fn_end, '.pdf')
    ## make plot
    g <- ggplot(filter(df, type == w_type),
            aes(x = factor(value,
                           levels = 0:4,
                           labels = c('No',
                                      'Yes',
                                      'Salary more important',
                                      'Same',
                                      'Salary less important')),
                y = pct_within_treat,
                fill = factor(enrolcol_201311,
                              levels = c(0,1),
                              labels = c('Not enrolled','Enrolled')),
                label = paste0(round(pct_within_treat, 1),'%',
                               '\n[',
                               formatC(n, format = 'd', big.mark = ','),
                               ']'))) +
        geom_bar(stat = 'identity', position = 'dodge2') +
        geom_text(size = pt_to_mm(9),
                  family = 'Times New Roman',
                  lineheight = .8,
                  position = position_dodge(width = .9), vjust = -0.3) +
        facet_wrap(~ factor(outcome,
                            levels = plot_order,
                            labels = to_plot_names(plot_order)),
                   scales = 'free_x') +
        ylim(c(0,100)) +
        labs(title = NULL,
             subtitle = na_to_null(subtitle),
             x = NULL,
             y = 'Percentage') +
        theme_bw(base_size = 9) +
        theme(text = element_text(family = 'Times New Roman', size = 9),
              axis.title.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_text(size = 9),
              axis.title.y = element_text(size = 9),
              panel.grid.major.x = element_blank(),
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.text = element_text(size = 9),
              strip.text.x = element_text(size = 9),
              strip.background = element_rect(fill = 'white'))

    ## message
    proj_message(fn,'s')
    ## save
    ggsave(filename = fn,
       plot = g,
       device = 'pdf',
       path = file.path(fig_dir),
       width = width,
       height = height,
       units = 'in',
       dpi = 'retina')
}

coef_plot <- function(df, outcome_1, outcome_2, subgroups, fn,
                      facet_var = 'facet_var', switch_facet = FALSE,
                      width = 7.5, height = 5.25, theme_base_size = 12) {

    ## which is facet and which is factor
    split <- if(switch_facet) {
                 c(facet_var, 'group')
             } else {
                 c('group', facet_var)
             }
    ## get plot data frames into list
    plot_df_list <- map(c(outcome_1, outcome_2),
                        ~ df %>%
                            filter(outcome == .x,
                                   group %in% subgroups) %>%
                            separate(group, split, sep = '_'))
    ## want same y-axis scale for combo plots, so need to get abs(max(95CI))
    ## between both and set here
    max_ylim_1 <- max(plot_df_list[[1]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[1]] %>% pull(lo95) %>% min))
    max_ylim_2 <- max(plot_df_list[[2]] %>% pull(hi95) %>% max,
                      abs(plot_df_list[[2]] %>% pull(lo95) %>% min))
    abs_max <- max(1, max_ylim_1, max_ylim_2)
    ylimits <- c((abs_max * -1) - (abs_max * .05),
                 abs_max + (abs_max * .05))
    yseq_unit <- case_when(
        grepl('jb', fn) ~ 1,
        grepl('vv', fn) ~ 0.5
        )
    gg <- list()
    for (i in 1:2) {
        plot_df <- plot_df_list[[i]]
        ptitle <- case_when(
            grepl('vot_reg', plot_df$outcome) ~ to_plot_names('vot_reg'),
            grepl('vol_ind', plot_df$outcome) ~ to_plot_names('vol_ind'),
            grepl('job_bal', plot_df$outcome) ~ to_plot_names('job_bal'),
            grepl('job_con', plot_df$outcome) ~ to_plot_names('job_con')
        )
        ## y label
        ylab <- case_when(
            grepl('vot_reg', plot_df$outcome) ~ 'Percentage point change',
            grepl('vol_ind', plot_df$outcome) ~ 'Percentage point change',
            grepl('job_bal', plot_df$outcome) ~ '(-) Salary / (+) Balance',
            grepl('job_con', plot_df$outcome) ~ '(-) Salary / (+) Contribution'
        )
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
        ## get observation data frame (prevent overplotting of label)
        plot_df_obs <- group_by(plot_df,
                                !!sym(facet_var)) %>%
            filter(row_number() == 1)
        ## plot
        gg[[i]] <- ggplot(plot_df, aes(x = x_run,
                                       y = est, colour = group,
                                       shape = group,
                                       label = paste0(round(est, 2), '\n(',
                                                      pval_func(est,se), ')'))) +
            geom_hline(yintercept = 0, linetype = 'dashed') +
            geom_linerange(aes(ymin = lo95, ymax = hi95), size = 0.5) +
            facet_wrap(as.formula(paste('~', facet_var))) +
            geom_point(fill = 'white', size = pt_to_mm(8)) +
            geom_point(size = pt_to_mm(8)) +
            {
                sub_plot_df <- subset(plot_df, est > 0)
                if (nrow(sub_plot_df) > 0) {
                    geom_text_repel(data = sub_plot_df,
                                    ylim = c(0,NA),
                                    ## use full df for cases when all sub_ are -/+
                                    xlim = c(min(plot_df$x_run),
                                             max(plot_df$x_run)),
                                    nudge_x = -0.01,
                                    colour = 'black',
                                    point.padding = 0.5,
                                    segment.color = 'grey50',
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(8),
                                    family = 'Times New Roman',
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
                                    nudge_x = -0.03,
                                    colour = 'black',
                                    point.padding = 0.5,
                                    segment.color = 'grey50',
                                    segment.alpha = 1,
                                    segment.size = .3,
                                    size = pt_to_mm(8),
                                    family = 'Times New Roman',
                                    force = 10,
                                    lineheight = .8)
                }
            } +
            scale_y_continuous(breaks = scales::pretty_breaks(),
                               limits = ylimits) +
            scale_shape_manual(values = c(15:18,22:24)) +
            coord_cartesian(clip = 'off') +
            geom_text(aes(label = formatC(obs, format = 'd', big.mark = ',')),
                      y = min(ylimits) - .2 * abs(min(ylimits)),
                      colour = 'black',
                      family = 'Times New Roman',
                      size = pt_to_mm(8)) +
            labs(y = ylab,
                 x = NULL,
                 title = ptitle) +
            theme_bw(base_size = theme_base_size) +
            guides(shape = NULL) +
            theme(text = element_text(family = 'Times New Roman', size = 9),
                  axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  strip.text.x = element_text(size = 9),
                  strip.background = element_rect(fill = 'white'),
                  legend.text = element_text(size = 9),
                  legend.title = element_blank())
    }
    ## combine plots
    g <- ggpubr::ggarrange(gg[[1]],
                           gg[[2]],
                           ncol = 1,
                           common.legend = TRUE,
                           legend = 'bottom')


    ## message
    proj_message(paste0(fn, '.pdf'),'s')
    ## save
    ggsave(filename = paste0(fn, '.pdf'),
           plot = g,
           device = 'pdf',
           path = fig_dir,
           width = width,
           height = height,
           units = 'in',
           dpi = 'retina')
}

## -----------------------------------------------
## plot: discrete bar plots
## -----------------------------------------------

proj_message('Reading discrete descriptive stats into memory')
df_d <- readRDS(file.path(des_dir, 'desc_discrete.rds')) %>%
    filter(outcome %in% c('vot_reg','vol_ind','job_bal','job_con')) %>%
    mutate(value = case_when(
               outcome %in% c('job_bal','job_con') & value == -1 ~ 2L,
               outcome %in% c('job_bal','job_con') & value == 0 ~ 3L,
               outcome %in% c('job_bal','job_con') & value == 1 ~ 4L,
               TRUE ~ value
           ))

## make and save plots
bar_plot_disc(df_d, 'unweighted')

## -----------------------------------------------
## read in analysis estimates
## -----------------------------------------------

proj_message('Reading estimates into memory')
files <- list.files(file.path(est_dir), '*.csv', full.names = TRUE)
fname <- get_basename(files) %>% gsub('_est', '', .)
df <- map(files, ~ read_csv(.x,
                            col_types = cols(group = 'c',
                                             outcome = 'c',
                                             .default = 'd'))) %>%
    bind_rows

proj_message('Saving plots at: ', fig_dir)

## -----------------------------------------------
## plot: gender X race
## -----------------------------------------------

subgroups <- grep('*_f$|*_m$', distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep('pov|non', subgroups, value = TRUE, invert = TRUE)

## vote/volunteering
coef_plot(df, 'vot_reg_cf_vi', 'vol_ind_cf_vi', subgroups, 'vv_gender_race')

## job
coef_plot(df, 'job_bal_cf_vi', 'job_con_cf_vi', subgroups, 'jb_gender_race')

## -----------------------------------------------
## plot: poverty X race
## -----------------------------------------------

subgroups <- grep('^pov', distinct(df, group) %>% pull, value = TRUE)
subgroups <- grep('_m_|_f_|_m$|_f$|non', subgroups, value = TRUE, invert = TRUE)
subgroups <- grep('_', subgroups, value = TRUE, invert = FALSE)

## vote/volunteering
coef_plot(df, 'vot_reg_cf_vi', 'vol_ind_cf_vi', subgroups, 'vv_poverty_race',
          switch_facet = TRUE)

## job
coef_plot(df, 'job_bal_cf_vi', 'job_con_cf_vi', subgroups, 'jb_poverty_race',
          switch_facet = TRUE)

## -----------------------------------------------------------------------------
## END SCRIPT
################################################################################
