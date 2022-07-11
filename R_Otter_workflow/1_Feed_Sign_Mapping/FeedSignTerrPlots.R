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
set.seed(123)
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
# RivOtter_OS # R Otter Survey data for Dwellings and Dams

# Filter unreliable historic data
RivOtter_FS <- RivOtter_FeedSigns %>%
filter(SurveySeason!= "Pre 2015")

RivOtter_OS <- RivOtter_OtherSigns %>%
  filter(SurveySeason!= "Pre 2015")


# ----- generate extent and titles for mapping ------
target_ext <- inflate_bbox(RivOtter_Catch_Area, 10)
plot_names <- unique(RivOtter_FS$SurveySeason)

# ------- get Open Street Map River network ---------
OSM_rivers <- get_rivers(RivOtter_Catch_Area)

# --------------- Feeding Sign Maps -----------------
fs_ggplot <- function(.data, pNames){
      fs <- plot_feeding(.data, weight_aes = c(0.75,1.5,3), basemap = F, axes_units = F, north_arrow = FALSE, scalebar = FALSE,
                         catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, guide=T,
                         guide_pos='bottom', wgs = F, mask_fill='grey80') +
        labs(subtitle =  pNames) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())
}

# generate list of confirmatory sign tibbles - split by survey season
Otter_fs_list <- RivOtter_FS %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status

feed_panel <- Otter_fs_list %>%
  purrr::map2(.x=., .y= plot_names,  ~fs_ggplot(.x, .y))%>%
  panel_plot(., guide=TRUE, n_col = 3) %>%
  ggsave('R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterFeedSigns.png',plot = .,
         dpi=300, height=180, width=180, units='mm')

# ------------- Confirmatory sign Maps --------------
conf_ggplot <- function(.data, pNames){
  leg <- FALSE
  if (pNames == '2019 - 2020'){
    leg <- TRUE
  }

  fs <- plot_other_signs(.data, size = 1.5, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE,
                     catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, guide=leg, guide_pos='bottom',
                     mask_fill='grey80') +
    labs(subtitle =  pNames) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
}

# generate list of confirmatory sign tibbles - split by survey season
Otter_conf_list <- RivOtter_OS %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status
other_panel <- Otter_conf_list %>%
  purrr::map2(.x=., .y= plot_names,  ~conf_ggplot(.x, .y))%>%
  panel_plot(., guide=TRUE, n_col = 3) %>%
  ggsave('R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterOtherSigns.png',plot = .,
       dpi=300, height=180, width=180, units='mm')


# -------------- Feeding Density Maps ---------------

fsd_ggplot <- function(.data, p.names){
  plot_forage_density(.data, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE, guide = TRUE,
                             catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext, attribute=FALSE,
                      mask_fill = "grey80")+
    labs(subtitle = p.names) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_blank()) +
    guides(fill = guide_colourbar(title.vjust = 1, barheight = 0.5, title='Forage density (low-high)', ticks = FALSE))
}

kde_general_func <- function(kbw, lt){
  RivOtter_FS %>%
    group_by(SurveySeason) %>%
    group_split()%>%
    purrr::map(., ~forage_density(., 'FeedCat', kd_extent = target_ext, grid_size=20, kern_bw = kbw, low_thresh = lt))
}

kde_ras_list <- kde_general_func(200, 1e-10)

kde_ras_listVIS <- kde_general_func(250, 1e-12)

kde_panel <- function(kde_list, filename){
  kde_list%>%
    purrr::map2(.x=., .y=plot_names, ~fsd_ggplot(.x, .y)) %>%
    panel_plot(., guide=TRUE, n_col = 3) %>%
    ggsave(filename=filename,plot = .,
           dpi=300, height=180, width=180, units='mm')
}

kde_panel(kde_ras_list, 'R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterFeedDensitySI.png') # for SI material - harder to read
kde_panel(kde_ras_listVIS, 'R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterFeedDensity.png') # exaggerated version for main paper.

# ---------- Territory Classification Maps ----------
terr_ggplot <- function(.data, pNames, fill.name, .lab, buffer=NULL){
  if (!is.null(buffer)){
    .data<-sf::st_buffer(.data, 50)
  }
  n.terr <- filter(.data, user_class =='Territory') %>%
    nrow()

      terr <- plot_territories(.data, fill.name, basemap = F, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE,
                               catchment = RivOtter_Catch_Area, rivers = OSM_rivers, plot_extent = target_ext,
                               guide_pos='bottom', label = .lab, mask_fill = 'grey80') +
        labs(subtitle =  pNames) +
        annotate("text", -Inf, Inf,
                 label =sprintf("n territories = %s", n.terr),
                  hjust = 0.1, vjust = 0.7) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        guides(fill = guide_legend(title='Home range class'))

}

# generate list of confirmatory sign tibbles - split by survey season
Otter_confirm_list <- RivOtter_OS %>%
  group_by(SurveySeason) %>%
  group_split()

# generate list of sf objects with estimated territory locations/status
auto_terr_list <- kde_ras_list %>%
  purrr::map2(.x = ., .y= Otter_confirm_list,
              .f = ~ estimate_territories(.x, .y, upper_thresh =0.95))

# check automated territory locations

auto_terr_list %>%
  purrr::map2(.x=., .y=plot_names, ~terr_ggplot(.x, .y, 'terr_status', TRUE))


# panel_plot(terr_panel, guide=TRUE, n_col = 4)%>%
# ggsave('R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterTerrsAuto.png',plot = ., dpi=300, height=7, width=10)

# edit automated classifications
terr_reclass <- list(#c(), # Pre 2015
                     c(), # 2015 - 2016
                     c(3), # 2016 - 2017
                     c(2, 7), # 2017 - 2018
                     c(3, 18, 20), # 2018 - 2019 Bicton + Otterton
                     c(3, 13, 16), # 2019 - 2020 Bicton + Otterton
                     c(16, 24, 11, 34)) # 2020 - 2021  Bicton + Otterton, 3 on the Tale
                                           # (DC, Not Mapped, tiny pond) (3, 34, 18, 26,

poss_reclass <- list(#c(11), # Pre 2015 - n11 downgraded as this was absent in 2015 and ind. known to have established at n8.
                     c(10), # 2015 - 2016
                     c(), # 2016 - 2017
                     c(10), # 2017 - 2018
                     c(5), # 2018 - 2019
                     c(9), # 2019 - 2020
                     c()) # 2020 - 2021

# reclass territories where needed...
obj_list <- dplyr::lst(terr_l = auto_terr_list, terr_re = terr_reclass, poss_re = poss_reclass)
reclass_terr_list <- purrr::pmap(obj_list, ~user_classify(..1, ..2, ..3))
saveRDS(reclass_terr_list, file='R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list2.Rds')

# plot corrected territory classes
User_terr_panel <- function(buff, filname){
  reclass_terr_list %>%
    purrr::map2(.x=., .y=plot_names, ~terr_ggplot(.x, .y, 'user_class', FALSE, buffer=buff))%>%
    panel_plot(., guide=TRUE, n_col=3) %>%
    ggsave(filname, plot = .,
           dpi=300, height=180, width=180, units='mm')
}

User_terr_panel(NULL, 'R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterTerrsUserSI.png')
User_terr_panel(100, 'R_Otter_workflow/1_Feed_Sign_Mapping/maps/OtterTerrsUser.png')

# reclass_terr_list %>%
#   lapply(st_drop_geometry) %>%
#   bind_rows(.id="year.id") %>%
#   group_by(year.id) %>%
#   filter(user_class =='Territory') %>%
#   summarise(n = n())

# Animation...
# ---- side by side animation ------------

# function to plot and combine with {patchwork} the kde and territory plots
sidebyside_ggplot <- function(.feeding, .kde, .terr, p.name){

  feed_points <- plot_feeding(.feeding, basemap = F, guide = TRUE,
                              catchment = RivOtter_Catch_Area, rivers=OSM_rivers,
                              plot_extent = target_ext, axes_units = FALSE,
                              north_arrow = F, scalebar = F, mask_fill = 'grey80')+
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())

  fsd <- plot_forage_density(.kde, basemap = F, guide = TRUE,
                             catchment = RivOtter_Catch_Area, rivers=OSM_rivers,
                             plot_extent = target_ext, axes_units = FALSE,
                             guide_width=0.5, north_arrow = F, scalebar = F, mask_fill = 'grey80') +
    ggplot2::scale_fill_viridis_c(na.value = NA, name= sprintf('%s Forage Density', 'log10'),
                                  trans='log10', n.breaks=3, limits=c(1.1e-10, 3.2e-06)) +
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text=element_blank()) +
    guides(fill = guide_colourbar(title.vjust = 1, barheight = 0.5, title='Forage density (low-high)', ticks = FALSE))

  terr <- plot_territories(.terr, basemap = F,'user_class', guide = TRUE,
                           catchment = RivOtter_Catch_Area,rivers=OSM_rivers,
                           plot_extent = target_ext, axes_units = FALSE,
                           mask_fill = 'grey80') +
    theme(legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    guides(fill = guide_legend(title='Home range class'))

  combined <- feed_points + fsd + terr
  combined <- combined  + plot_annotation(
    subtitle =  p.name)
  return(combined)

}

# sidebyside_ggplot(Otter_fs_list[[6]],
#                   kde_ras_list[[6]],
#                   reclass_terr_list[[6]],•
#                   plot_names[[6]]) +
#   ggsave('R_Otter_workflow/1_Feed_Sign_Mapping/maps/SbSTest.png', dpi=300, height=7, width=12)

# create list of rasters, territrories and names for the map funtion
obj_list <- list(Otter_fs_list, kde_ras_list, reclass_terr_list, plot_names)

# map the above function to the list and animate it.
joint_animation <- purrr::pmap(.l=obj_list, ~sidebyside_ggplot(..1, ..2, ..3, ..4))

animate_maps(joint_animation, filename = 'R_Otter_workflow/1_Feed_Sign_Mapping/maps/AnimatedFeeding.gif')
