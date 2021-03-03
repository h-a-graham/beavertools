## ============== BEAVER FIELD SIGN AND TERRITORY VISUALISATION ============#
# Written by Hugh Graham, 2021.
# This script uses the {beavertools} package to visualise the change in beaver
# population within the River Otter (Devon)
#===========================================================================#
# Notes:
# set up license and add dependencies to description.
# generate publication plots for the feeding points, confirmatory signs,
# Feeding density, territories.
#===========================================================================#

# --------- Imports -----------------------
library(patchwork)
library(sf)
library(magrittr)
library(tidyverse)
library(patchwork)
library(ggfortify)
devtools::load_all()
# devtools::document()

#  -------------------- Data ------------------------
# RivOtter_Catch_Area # R Otter Catchment area - included with {beavertools}
# RivOtter_FeedSigns # R Otter Feeding Sign survey data
# RivOtter_OtherSigns # R Otter Survey data for Dwellings and Dams

# ----- generate extent and titles for mapping ------
target_ext <- inflate_bbox(RivOtter_Catch_Area, 10)
plot_names <- unique(RivOtter_FeedSigns$SurveySeason)

# ------- get Open Street Map River network ---------
OSM_rivers <- get_rivers(RivOtter_Catch_Area)

# --------------- Feeding Sign Maps -----------------
fs_ggplot <- function(.data, pNames){
      fs <- plot_feeding(.data, weight_aes = c(0.75,1.5,3), basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE,
                         catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, guide=T, guide_pos='bottom') +
        labs(subtitle =  pNames) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
}

# generate list of confirmatory sign tibbles - split by survey season
Otter_fs_list <- RivOtter_FeedSigns %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status

feed_panel <- Otter_fs_list %>%
  purrr::map2(.x=., .y= plot_names,  ~fs_ggplot(.x, .y))%>%
  panel_plot(., guide=TRUE)
ggsave('run/paper_plots/maps/OtterFeedSignsNBM.png',plot = feed_panel, dpi=300, height=7, width=7)

# ------------- Confirmatory sign Maps --------------
conf_ggplot <- function(.data, pNames){
  leg <- FALSE
  if (pNames == '2019 - 2020'){
    leg <- TRUE
  }

  fs <- plot_other_signs(.data, size = 1.5, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE,
                     catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, guide=leg, guide_pos='bottom') +
    labs(subtitle =  pNames) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# generate list of confirmatory sign tibbles - split by survey season
Otter_conf_list <- RivOtter_OtherSigns %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status
other_panel <- Otter_conf_list %>%
  purrr::map2(.x=., .y= plot_names,  ~conf_ggplot(.x, .y))%>%
  panel_plot(., guide=TRUE)
ggsave('run/paper_plots/maps/OtterOtherSigns.png',plot = other_panel, dpi=300, height=7, width=7)


# -------------- Feeding Density Maps ---------------
fsd_ggplot <- function(.data, p.names){
  plot_forage_density(.data, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE, guide = TRUE,
                             catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, attribute=FALSE,
                      guide_width=0.5)+
    labs(subtitle = p.names) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

kde_ras_list <- RivOtter_FeedSigns %>%
  group_by(SurveySeason) %>%
  group_split()%>%
  purrr::map(., ~forage_density(., 'FeedCat', kd_extent = target_ext))


kde_panel <- kde_ras_list %>%
  purrr::map2(.x=., .y=plot_names, ~fsd_ggplot(.x, .y)) %>%
  panel_plot(., guide=TRUE)
ggsave('run/paper_plots/maps/OtterFeedDensity.png',plot = kde_panel, dpi=300, height=7, width=7)

# ---------- Territory Classification Maps ----------
terr_ggplot <- function(.data, pNames, fill.name, .lab ){

      terr <- plot_territories(.data, fill.name, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE,
                               catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, guide_pos='bottom', label = .lab) +
        labs(subtitle =  pNames) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

}

# generate list of confirmatory sign tibbles - split by survey season
Otter_confirm_list <- RivOtter_OtherSigns %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status
auto_terr_list <- kde_ras_list %>%
  purrr::map2(.x = ., .y= Otter_confirm_list,
              .f = ~ estimate_territories(.x, .y))

# create panel plot of territory locations
terr_panel <- auto_terr_list %>%
  purrr::map2(.x=., .y=plot_names, ~terr_ggplot(.x, .y, 'terr_status', TRUE))%>%
  panel_plot(., guide=TRUE)
ggsave('run/paper_plots/maps/OtterTerrsAuto.png',plot = terr_panel, dpi=300, height=7, width=7)

# edit automated classifications
terr_reclass <- list(c(2), # Pre 2015
                     c(3, 5), # 2015 - 2016
                     c(), # 2016 - 2017
                     c(14, 15), # 2017 - 2018
                     c(6,19), # 2018 - 2019
                     c(12, 31, 32)) # 2019 - 2020

poss_reclass <- list(c(), # Pre 2015
                     c(), # 2015 - 2016
                     c(), # 2016 - 2017
                     c(), # 2017 - 2018
                     c(), # 2018 - 2019
                     c()) # 2019 - 2020

# reclass territories where needed...
obj_list <- dplyr::lst(terr_l = auto_terr_list, terr_re = terr_reclass, poss_re = poss_reclass)
reclass_terr_list <- purrr::pmap(obj_list, ~user_classify(..1, ..2, ..3))

# plot corrected territory classes
User_terr_panel <- reclass_terr_list %>%
  purrr::map2(.x=., .y=plot_names, ~terr_ggplot(.x, .y, 'user_class', FALSE))%>%
  panel_plot(., guide=TRUE)
ggsave('run/paper_plots/maps/OtterTerrsUser.png',plot = User_terr_panel, dpi=300, height=7, width=7)


# ----- plot increases in territory numbers  #### MOVE THIS SECTION TO A NEW SCRIPT.

# What is the capacity of the catchment?
lower_capacity = 89
upper_capacity = 150

# set up dataframe for observed territory counts...
date_list <- lubridate::dmy(c("30-12-2014", "30-12-2015", "30-12-2016",
                              "30-12-2017", "30-12-2018", "30-12-2019"))
years_since_release <- c(7:12)

get_terr_counts <- function(terr_map, .season, .year, .ysr){
  terr_map %>%
    dplyr::filter(user_class == 'Territory') %>%
    summarise(terr_count = n(), season= .season, year = .year, years_since = .ysr)

}

count_obj_list <- list(reclass_terr_list, plot_names, date_list, years_since_release)
terr_counts <-  purrr::pmap(count_obj_list, ~get_terr_counts(..1, ..2, ..3, ..4)) %>%
  bind_rows() %>%
  mutate(year_adj = years_since + 2007)

# fit logistic model to determine the fit up to 50% capacity

.logmodel <- glm(terr_count ~ years_since, family=poisson(link='log'), data=terr_counts)

autoplot(.logmodel, which = 1:6, label.size = 3)
summary(.logmodel)
broom::tidy(.logmodel, exponentiate=T, conf.int=T)


# get half capacity prediction date...
HalfCap <- function(cap){ tibble(years_since = seq(0,50, by=0.1)) %>%
  broom::augment(.logmodel, newdata=., se_fit=T, type.predict = "response",
                 type.residuals = "deviance") %>%
  mutate(.fitround = floor(.fitted)) %>%
  filter(.fitround==round(cap/2)) %>%
  pull(years_since)
}

#create new df with anchor points.
hacked_df_func <- function(cap){
  terr_counts %>%
    sf::st_drop_geometry() %>%
    bind_rows(tibble(terr_count=round(cap), season="Full Cap",
                     years_since=HalfCap(cap) * 2, year_adj=years_since + 2007))%>%
    bind_rows(tibble(terr_count=round(cap), season="Future Full Cap",
                     years_since=HalfCap(cap) * 2 + 50, year_adj=years_since + 2007))

}



# function to fit new spline
fit_n_predict <- function(df, cap, cap_level){
  # fit new model
  .splinemod <- glm(terr_count ~ splines::ns(years_since,df=1, knots=c(HalfCap(cap)),
                                             Boundary.knots=c(0, HalfCap(cap)*2)),
                    family=poisson(link='log'), data=df)

  # create new data with predictions
  new_data <- tibble(years_since = seq(0,50, by=0.1)) %>%
    broom::augment(.splinemod, newdata=., se_fit=T, type.predict = "response",
                   type.residuals = "deviance") %>%
    mutate(conf.low = .fitted - (.se.fit*1.96),
           conf.high = .fitted + (.se.fit*1.96)) %>%
    mutate(.fitted = ifelse(.fitted>cap, cap, .fitted),
           conf.low = ifelse(conf.low < 0, 0, conf.low),
           conf.high = ifelse(conf.high > cap, cap, conf.high),
           year_adj = years_since + 2007,
           cap_name = cap_level) #%>%
    # bind_cols(broom::augment(.logmodel, newdata=tibble(years_since = seq(0,50, by=0.1)), type.predict = "response",
    #                          type.residuals = "deviance") %>%
    #             rename(.log.fitted=.fitted) %>%
    #             select(.log.fitted))
}

hacked_df <- hacked_df_func(lower_capacity) %>%
  fit_n_predict(., lower_capacity, "Open Network") %>%
  bind_rows(., hacked_df_func(upper_capacity) %>%
              fit_n_predict(., upper_capacity, "MasterMap Network"))


terr_pred <- ggplot(hacked_df, aes(x=year_adj, y=.fitted, colour=cap_name, fill=cap_name))+
  geom_hline(yintercept=lower_capacity, linetype=4, lwd=0.3)+
  geom_hline(yintercept=upper_capacity, linetype=4, lwd=0.3)+
  # geom_line(data=new_data, aes(y = .log.fitted), colour='#000E54', lwd=1.2, linetype=3) +
  geom_line(lwd=1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha=0.2, linetype=2, lwd=0.2) +
  geom_point(data=terr_counts, aes(x=year_adj, y=terr_count), inherit.aes = F, alpha=0.8)+
  # stat_smooth(method="lm",formula='y ~ poly(x, 2)', fullrange=TRUE, colour='#43B1FF', se=F) +
  # geom_smooth(method = "glm", formula = y~x,
  #               method.args = list(family = poisson(link = 'log')), fullrange=TRUE) +

  # geom_vline(xintercept=HalfCap, linetype=4, lwd=0.3)+
  annotate("text", x=2015.5, y = lower_capacity + 3, label = "Predicted territory capacity: Open Network", size=3) +
  annotate("text", x=2015, y = upper_capacity + 3, label = "Predicted territory capacity: MM Network", size=3) +
  scale_fill_manual(values = c("#d95f02", "#7570b3"), name=NULL)+
  scale_colour_manual(values = c("#d95f02", "#7570b3"), name=NULL)+
  # xlim(lubridate::dmy(c("30-12-2007", "30-12-2050"))) +
  # xlim(0,100) +
  # ylim(0,1000) +
  # coord_cartesian(ylim=c(0,100), xlim = c(0,50))+
  coord_cartesian(ylim=c(0,155), xlim = c(2007,2045))+
  # coord_cartesian(ylim=c(0,100), xlim = lubridate::dmy(c("30-12-2007", "30-12-2031")))+
  labs(x = 'year', y="n territories", subtitle="Beaver territory expansion in R. Otter Catchment???")+
  theme_bw() +
  theme(legend.position = "bottom")
terr_pred
ggsave('run/paper_plots/plots/Territory_predictionMMvsORN.png',plot = terr_pred, dpi=300, height=7, width=7)



# Animation...
# ---- side by side animation ------------

# function to plot and combine with {patchwork} the kde and territory plots
sidebyside_ggplot <- function(.feeding, .kde, .terr, p.name){

  feed_points <- plot_feeding(.feeding, basemap = F, guide = TRUE, catchment = RivOtter_Catch_Area, rivers=OSM_rivers,
                              plot_extent = target_ext, axes_units = FALSE, north_arrow = F, scalebar = F)+
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  fsd <- plot_forage_density(.kde, basemap = F, guide = TRUE, catchment = RivOtter_Catch_Area, rivers=OSM_rivers,
                             plot_extent = target_ext, axes_units = FALSE, guide_width=0.5, north_arrow = F, scalebar = F) +
    ggplot2::scale_fill_viridis_c(na.value = NA, name= sprintf('%s Forage Density', 'log10'), trans='log10',
                                    n.breaks=3, limits=c(1.1e-10, 3.2e-06)) +
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  terr <- plot_territories(.terr, basemap = F,'user_class', guide = TRUE, catchment = RivOtter_Catch_Area,rivers=OSM_rivers,
                           plot_extent = target_ext, axes_units = FALSE) +
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(fill=guide_legend(title="Territory status"))

  combined <- feed_points + fsd + terr
  combined <- combined  + plot_annotation(
    subtitle =  p.name)
  return(combined)

}

sidebyside_ggplot(Otter_fs_list[[6]],
                  kde_ras_list[[6]],
                  reclass_terr_list[[6]],
                  plot_names[[6]]) +
  ggsave('run/paper_plots/maps/SbSTest.png', dpi=300, height=7, width=12)

# create list of rasters, territrories and names for the map funtion
obj_list <- list(Otter_fs_list, kde_ras_list, reclass_terr_list, plot_names)

# map the above function to the list and animate it.
joint_animation <- purrr::pmap(.l=obj_list, ~sidebyside_ggplot(..1, ..2, ..3, ..4))

animate_maps(joint_animation, filename = 'run/paper_plots/maps/AnimatedFeeding.gif')
