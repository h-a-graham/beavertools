library(patchwork)
library(sf)
library(magrittr)
library(tidyverse)
library(patchwork)
library(ggfortify)
library(investr)
library(colorspace)
library(grid)
library(gtable)
# devtools::load_all()
library(beavertools)
#----- Define some directories -----------------

plot_dir <- file.path(here::here(),"R_Otter_workflow/3_Pop_expansion_predictions/plots")
sim_dir <- file.path(here::here(),"R_Otter_workflow/2_Territory_simulations/exports")
# ---- Read in Data Territroy count data --------------
reclass_terr_list <- readRDS(file='R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list2.Rds')
# get unique names for survey years...
plot_names <- RivOtter_FeedSigns %>%
  filter(SurveySeason!= "Pre 2015") %>%
  pull(SurveySeason) %>% unique()

# What is the capacity of the catchment? WILL NEED UPDATING WITH RANGES ETC WHEN SIMULATIONS ARE DONE.
cap_limits <- read_rds(file.path(sim_dir, 'sim_terr.Rds')) %>%
  sf::st_drop_geometry() %>%
  summarise(lowest = min(n),
            highest = max(n))
lower_capacity <- cap_limits$lowest
upper_capacity <- cap_limits$highest

#  ------- set up dataframe for observed territory counts... -------
# date_list <- lubridate::dmy(c("30-12-2014", "30-12-2015", "30-12-2016",
#                               "30-12-2017", "30-12-2018", "30-12-2019",
#                               "30-12-2020"))
# years_since_release <- c(8:14)
date_list <- lubridate::dmy(c("30-12-2015", "30-12-2016",
                              "30-12-2017", "30-12-2018", "30-12-2019",
                              "30-12-2020"))
years_since_release <- c(8:13)
# years_since_release <- c(1:6)
get_terr_counts <- function(terr_map, .season, .year, .ysr){
  terr_map %>%
    dplyr::filter(user_class == 'Territory') %>%
    summarise(terr_count = n(), season= .season, year = .year, years_since = .ysr)

}

count_obj_list <- list(reclass_terr_list, plot_names, date_list, years_since_release)
terr_counts <-  purrr::pmap(count_obj_list, ~get_terr_counts(..1, ..2, ..3, ..4)) %>%
  bind_rows() %>%
  mutate(year_adj = years_since + 2007) %>% #2007
  mutate(terr_count = ifelse(season %in% c('2018 - 2019', '2019 - 2020'), terr_count+1,
                             ifelse(season %in% c('2020 - 2021'),terr_count + 2, terr_count))) %>% # required because some territories not correctly identified due to semi-automate process.
  mutate(name = 'Observed Data')

# reclass_terr_list[[1]] %>% # for double checking only.
#   filter(user_class == "Territory") %>%
#   ggplot(., aes(fill=(as.character(id)))) +
#   geom_sf()

# --- fit log glm model to determine the fit up to 50% capacity ----------

.logmodel <- glm(terr_count ~ years_since, family=poisson(link='log'), data=terr_counts)

# --- visualise initial model ------
tibble(years_since  = seq(0,50, by=0.01)) %>%
  broom::augment(.logmodel, newdata=., se_fit=T, type.predict = "response") %>%
  ggplot(., aes(x=years_since, y = .fitted))+
  geom_point(data=terr_counts, aes(x=years_since, y=terr_count), inherit.aes = F, alpha=0.6)+
  geom_line() +
  geom_ribbon(aes(ymin=.fitted -(1.96*.se.fit),ymax=.fitted +(1.96*.se.fit) ), alpha=0.3)+
  coord_cartesian(ylim=c(0,upper_capacity +10))+
  theme_bw()


## ----- uncomment for model diagnosticss. -------
# autoplot(.logmodel, which = 1:6, label.size = 3)
# summary(.logmodel)
# broom::tidy(.logmodel, exponentiate=T, conf.int=T)


# ----- functions to fit logistic growth model -------

source(file.path(here::here(),"R_Otter_workflow/3_Pop_expansion_predictions", 'logistic_growth.R'))

# generate logistic models...
hacked_df <-  seq(lower_capacity, upper_capacity, by=1) %>%
  purrr::map(., ~logistic_growth(., .logmodel, st.date=2007)) %>%
  bind_rows()

# function to create capcity ribbon for plot.
ribbon_df <- function() tibble(year_adj = seq(2000,2070, by=70),.fitted = seq(0,200, by=200))

mid_cap <- function() lower_capacity + ((upper_capacity-lower_capacity)*0.5)


# create plot.
hacked_df %>%
  # filter(cap_name==115)%>%
  ggplot(., aes(x=year_adj, y=.fitted))+

  ### This bit adds the ribbon to show estimated maximum  terr. capacity.
  # geom_ribbon(data=ribbon_df(), aes(ymin=lower_capacity, ymax=upper_capacity, xmin=2000),
  #             fill='grey90', size=0.1, alpha=0.2, linetype=2, color="grey10") +
  # annotate("text", x=2020, y = mid_cap(),
  #          label = "predicted territory capacity range", size=3) +
  # geom_segment(aes(x = 2020, y = mid_cap() + 5, xend = 2020, yend = upper_capacity -1),
  #              arrow = arrow(length = unit(0.01, "npc")),lwd=0.5, color="grey20") +
  # geom_segment(aes(x = 2020, y = mid_cap() - 5, xend = 2020, yend = lower_capacity +1),
  # arrow = arrow(length = unit(0.01, "npc")), lwd=0.5, color="grey20") +

  ### This gives just the lines
  # geom_line(aes(group=reorder(cap_name, rev(cap_name)), color=cap_name), lwd=0.5, alpha=0.9)+
  # stat_summary(fun = mean, geom = 'line', size=0.6, alpha=0.6, linetype=1, color="grey20") +
  # # stat_summary(fun = min, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +
  # # stat_summary(fun = max, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +
  # # scale_colour_viridis_c(option='turbo')+
  # scale_colour_continuous_sequential("Batlow", rev=F) +
  # guides(colour = guide_colourbar(barwidth = 8, barheight = 0.5, title="Territory Capacity", title.vjust=1)) +

  ## this generates the Confidence interval version (CIs are very debatable - safer to levave?)
geom_ribbon(aes(ymin=pred.lwr, ymax = pred.upr,
                group=reorder(cap_name, rev(cap_name))),
            fill='#18BFC2', alpha=0.04) +
  # geom_ribbon(aes(ymin=pred.lwr, ymax = pred.upr, group=reorder(cap_name, rev(cap_name)), fill=cap_name), colour=NA) +
  stat_summary(aes(y=pred.lwr), fun = min, geom = 'line', size=0.3, alpha=0.9, linetype=2, color="grey10") +
  stat_summary(aes(y=pred.upr),fun = max, geom = 'line', size=0.3, alpha=0.9, linetype=2, color="grey10") +
  # scale_fill_continuous_sequential("TealGrn", rev=F) +
  # guides(fill = guide_colourbar(barwidth = 8, barheight = 0.5, title="Territory Capacity")) +

  #adds mean model to plot
  stat_summary(fun = mean, geom = 'line', size=0.6, alpha=0.9, linetype=1, color="grey10") +
  # adds original data
  geom_point(data=terr_counts, aes(x=year_adj, y=terr_count), shape=21, size=1.5,
             stroke = 1.2)+
  # define plot style n stuff
  coord_cartesian(ylim=c(0,upper_capacity +5), xlim = c(2015, 2045))+
  labs(x = 'Year', y="Number of Territories")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0))) #+
  ggsave(file.path(plot_dir, 'TerritoryPredictiond2.png'),
         dpi=600, height=180, width=180, units='mm')


# summary to show the earliest and latest that capacity will be met (with CIs)

get_summ <- function(df, .fun1, .fun2, col_name, out_names){
  df %>%
    filter(cap_name == .fun1(hacked_df$cap_name),
           {{col_name}} >= lower_capacity,
           {{col_name}} <=upper_capacity) %>%
    summarise(!!out_names := .fun2(year_adj))
}

bind_cols(
get_summ(hacked_df, max, min, .fitted, 'early_mean'),
get_summ(hacked_df, min, max, .fitted, 'late_mean'),
get_summ(hacked_df, max, min, pred.upr, 'lower_CI'),
get_summ(hacked_df, min, max, pred.lwr, 'upper_CI')
)


# -------- pop dynamcis plots -----------

make_long <- function(df, col_list, nms_to, vals_to){
  df %>%
    pivot_longer(., cols={{col_list}},
                 names_to = nms_to, values_to = vals_to)
}

long_df <- bind_cols(
  hacked_df %>%
    make_long(col_list=c(growth_rate, n_terr_growth, hartman_rate),
              nms_to='mid', vals_to='mid.long') %>%
    mutate(mid = ifelse(mid=='growth_rate', "Expansion Rate",
                        ifelse(mid=="n_terr_growth","New Territories per Year",
                               "Expansion Rate / time"))),
  hacked_df %>%
    make_long(col_list=c(growth_rate.lwr, n_terr_growth.lwr, hartman_rate.lwr),
              nms_to='lwr', vals_to='lwr.long') %>%
    select(lwr.long),
  hacked_df %>%
    make_long(col_list=c(growth_rate.upr, n_terr_growth.upr, hartman_rate.upr),
              nms_to='upr', vals_to='upr.long') %>%
    select(upr.long)
) %>%
  mutate(density = .fitted/(as.numeric(sf::st_area(RivOtter_Catch_Area))/1e+6))



# long_df <- hacked_df %>%
#   pivot_longer(., cols=c(growth_rate, n_terr_growth, hartman_rate), names_to = "mid", values_to = 'mid.long')%>%
#   mutate(mid = ifelse(mid=='growth_rate', "Expansion Rate",
#                       ifelse(mid=="n_terr_growth","New Territories per Year", "Expansion Rate / time")),
#          density = .fitted/(as.numeric(sf::st_area(RivOtter_Catch_Area))/1e+6))

# function to generate the desired pop dynamics plots.
pop.dynams <- function(df, x_val, x_lab, leg_pos){
  ggplot(df, aes(x=!! dplyr::sym(x_val) , y=mid.long))+
    geom_ribbon(aes(ymin=lwr.long, ymax = upr.long,
                    group=c(reorder(cap_name, rev(cap_name)))),
                fill='#18BFC2', alpha=0.04, colour=NA) +

    # stat_summary(aes(y=lwr.long), fun = min, geom = 'line', size=0.3, alpha=0.6, linetype=2, color="grey20") +
    # stat_summary(aes(y=upr.long),fun = max, geom = 'line', size=0.3, alpha=0.6, linetype=2, color="grey20") +
    # stat_summary(aes(y=mid.long),fun = mean, geom = 'line', size=0.6, alpha=0.6, linetype=1, color="grey20") +
    geom_line(data=df[df$cap_name==round(lower_capacity +
                                           (upper_capacity-lower_capacity)/2),],
              aes(y=mid.long),size=0.5, alpha=0.9, linetype=1, color="grey20")+
    # geom_line(data=df[df$cap_name==upper_capacity,],aes(y=upr.long),size=0.3, alpha=0.6, linetype=2, color="grey20")+
    # geom_line(aes(group=c(reorder(cap_name, rev(cap_name)))), lwd=0.5, alpha=0.7) +
    # scale_colour_viridis_c(option='turbo')+
    # scale_colour_continuous_sequential("Batlow", rev=F) +
    # guides(colour = guide_colourbar(barwidth = 8, barheight = 0.5, title="Territory Capacity", title.vjust=1)) +
    labs(x = x_lab, y='') +
    theme_bw() +
    theme(legend.position = leg_pos,
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 6, l = 0))) +
    facet_wrap(~mid, scales = "free")
}

# create stacked plot.
pop_dyn_plot1 <- long_df %>%
  pop.dynams(., 'years_since', "Years since establishment", "none")+
  coord_cartesian( xlim = c(9, 45))
pop_dyn_plot2 <-long_df %>%
  pop.dynams(., 'density', expression(paste("Density ", (territories/km) ^2)), "bottom") +
  theme(strip.background = element_blank(), strip.text = element_blank())

pop_dyn_plot3 <- pop_dyn_plot1/pop_dyn_plot2
ggsave(file.path(plot_dir, 'TerritoryDynamics.png'),pop_dyn_plot3,
         dpi=600, height=180, width=180, units='mm')

# stats for paper:
long_df %>%
  filter(cap_name %in% c(120, 183)) %>%
  group_by(cap_name) %>%
  summarise(max_den = last(density))


# ------ management impacts ----------------
# This function fits the new management scenarios based on the time of management start and
# how many territories are removed each year.
source(file.path(here::here(), 'R_Otter_workflow/3_Pop_expansion_predictions/add_general_facet_labs.R'))

mgmt_scenario <- function(df, .mgmt_start, .mgmt_n_terrs) {

  gen_mgmt_curve <- function(mgmt_start, mgmt_n_terrs){
    growth_vec <- vector("double", length(nrow(df)))
    growth_vecL <- vector("double", length(nrow(df)))
    growth_vecU <- vector("double", length(nrow(df)))
    for (i in seq_len(nrow(df))) {
      if (df$year_adj[i] < mgmt_start){
        # g <- round(df$.fitted[i],6)
        g <- df$.fitted[i]
        gl <- df$pred.lwr[i]
        gu <- df$pred.upr[i]
      } else{
        g_rate <- df$n_terr_growth[which.min(abs(df$.fitted - g))]
        g <- g + g_rate - mgmt_n_terrs

        g_rateL <- df$n_terr_growth.lwr[which.min(abs(df$pred.lwr - gl))]
        gl <- gl + g_rateL - mgmt_n_terrs

        g_rateU <- df$n_terr_growth.upr[which.min(abs(df$pred.upr - gu))]
        gu <- gu + g_rateU - mgmt_n_terrs

      }
      growth_vec[i] <- g
      growth_vecL[i] <- gl
      growth_vecU[i] <- gu
    }
    return(tibble(mgmt_growth = growth_vec,
                  mgmt_growth.lwr = growth_vecL,
                  mgmt_growth.upr = growth_vecU,
                  mgmt_year = mgmt_start,
                  mgmt_removed = mgmt_n_terrs) %>%
             bind_cols(df))
  }
  dist_func <- function(.year, .nList){
    purrr::map(.nList, ~gen_mgmt_curve(.year, .)) %>%
      bind_rows()
  }
  purrr::map(.mgmt_start, ~dist_func(., .mgmt_n_terrs)) %>%
    bind_rows() #%>%
    # mutate_if(is.numeric, ~replace(., .<0, NA))
}

mgmt_df <- hacked_df %>%
  group_by(cap_name) %>%
  group_split() %>%
  purrr::map(., ~ mgmt_scenario(., c(2022, 2026, 2030), c(2, 3, 5, 8))) %>%
  bind_rows()

# sum stats for paper:
mgmt_df %>%
  # filter(mgmt_year %in% c(2022, 2025, 2035)) %>%
  filter(mgmt_removed == 5 | mgmt_removed == 7) %>%
  filter(cap_name == 120 |cap_name == 183) %>%
  group_by(mgmt_year, mgmt_removed, cap_name) %>%
  summarise(mgmt_cap = last(mgmt_growth), max_cap = last(.fitted)) %>%
  mutate(perc_decline = (max_cap-mgmt_cap)/max_cap*100)

# (120-106)/120*100###???

# create plot.
mgmt_plot <- function(df){
  df %>%
    # filter(cap_name==115)%>%
    ggplot(., aes(x=year_adj, y=.fitted))+

    # shows range of unmanaged scenario
    # stat_summary(fun = min, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +
    # stat_summary(fun = max, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +

    geom_ribbon(aes(ymin=mgmt_growth.lwr, ymax = mgmt_growth.upr,
                    group=reorder(cap_name, rev(cap_name))),
                fill='#18BFC2', alpha=0.04) +
    stat_summary(aes(y=mgmt_growth.lwr), fun = min, geom = 'line', size=0.3, alpha=0.9, linetype=1, color="grey10") +
    stat_summary(aes(y=mgmt_growth.upr),fun = max, geom = 'line', size=0.3, alpha=0.9, linetype=1, color="grey10") +
    # geom_ribbon(aes(ymin=pred.lwr, ymax = pred.upr, group=reorder(cap_name, rev(cap_name)), fill=cap_name), colour=NA) +
    stat_summary(aes(y=pred.lwr), fun = min, geom = 'line', size=0.3, alpha=0.9, linetype=2, color="grey10") +
    stat_summary(aes(y=pred.upr),fun = max, geom = 'line', size=0.3, alpha=0.9, linetype=2, color="grey10") +


    ### This gives just the lines
    # geom_line(aes(y=mgmt_growth, group=reorder(cap_name, rev(cap_name)), color=cap_name), lwd=0.5, alpha=0.5)+
    # scale_colour_viridis_c(option='turbo') +
    scale_colour_continuous_sequential("Batlow", rev=F) +
    guides(colour = guide_colourbar(barwidth = 8, barheight = 0.5, title="Territory Capacity", title.vjust=1)) +

    facet_grid(mgmt_removed ~ mgmt_year ) +

    # define plot style n stuff
    coord_cartesian(ylim=c(8,upper_capacity +5), xlim = c(2015, 2060))+
    labs(x = 'Year', y="Number of Territories")+
    theme_bw() +
    theme(legend.position = "bottom",
          axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)))
}

mgmt_p <- mgmt_plot(mgmt_df)

add_general_facet_labs(mgmt_p, 'n territories removed each year', ' Year managment starts') %>%
  ggsave(file.path(plot_dir, 'MgmtDynamics.png'), .,
         dpi=600, height=180, width=180, units='mm')


mgmt_df_big <- hacked_df %>%
  group_by(cap_name) %>%
  group_split() %>%
  purrr::map(., ~ mgmt_scenario(., seq(2022, 2030, by=2),
                                seq(2, 14, by=2))) %>%
  bind_rows()

mgmt_p_matrix <- mgmt_plot(mgmt_df_big) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 3))

mgmt_p_matrix2 <- add_general_facet_labs(mgmt_p_matrix, 'n territories removed each year', ' Year managment starts')

s <- 0.6
ggsave(file.path(plot_dir, 'MgmtDynamicsMatrix.png'), mgmt_p_matrix2,
         dpi=600, height=315*s, width=480*s, units='mm')
#

#------ SS -----------
# plotting option with ribbon.
# mgmt_df %>%
#   filter(cap_name %in% c(min(hacked_df$cap_name),max(hacked_df$cap_name))) %>%
#   mutate(cap_name = ifelse(cap_name == min(hacked_df$cap_name), 'Low', 'High')) %>%
#   select(cap_name, .fitted, year_adj, mgmt_scen) %>%
#   pivot_longer(., cols=c(.fitted, mgmt_scen), names_to = "type", values_to = 'value')%>%
#   pivot_wider(., names_from =cap_name, values_from = c(value)) %>%
#   mutate(type = ifelse(type=='.fitted', 'No Management', ' With Management'))%>%
#   ggplot(., aes(x=year_adj, fill=type)) +
#   geom_ribbon(aes(ymin=Low, ymax=High, linetype=type), alpha=0.7, colour='grey50') +
#   scale_fill_manual(values=c("#43FFB5", NA)) +
#   coord_cartesian(ylim=c(0,upper_capacity +5), xlim = c(2007, 2045))+
#   labs(x = 'Year', y="Number of Territories")+
#   theme_bw() +
#   theme(legend.position = "bottom",
#         legend.title = element_blank(),
#         axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)),
#         axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0)))


