
#Notes:
# Let's revisit the literature for mean territory sizes - need to consider if we're currently using
# a distribution which is either too narrow or too small - also is a normal distribution the right
# way to go? perhaps a uniform distribution is more appropriate?

# Would beavers use all branches to make the most of a territory in a pressured scenario or is their
# behaviour too dependent on linear movement?

# we should really include minBDC as a variable in the simulation to consider a range of possible values...



# ------------ imports --------------------
devtools::load_all()
# devtools::uninstall()
devtools::install()
# devtools::document()
library(tidyverse)
library(sf)
library(broom)
library(patchwork)
library(beavertools)
#-------------define folder root -------------

export_dir <- file.path(here::here(),"R_Otter_workflow/2_Territory_simulations/exports")
plot_dir <- file.path(here::here(),"R_Otter_workflow/2_Territory_simulations/plots")
# ----- generate extent and titles for mapping ------
target_ext <- inflate_bbox(RivOtter_Catch_Area, 10)
# ------------ Read Data ------------------

Real_terrs <- readRDS('R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list.Rds')

# MasterMap River Network not released with the package due to licensing issues.
MMRN_BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg') # MasterMap Data

# OS Open Rivers Network - included with the package
ORN_BeavNetOtter <- RivOtter_BeaverNet

# ---------- run terriroty generation for all reaches for each network
run_terr_gen <- function(riv_network, overwrite=FALSE, save_out=TRUE){
  fileName <- file.path(export_dir, paste('PT_', deparse(substitute(riv_network)),'.Rds', sep=""))
  # print(fileName)
  if (file.exists(fileName) && isFALSE(overwrite)){
    message(sprintf("Potential territrories created in: %s", fileName))
    message("Loading previously generated file...")
    terr_out <- readRDS(fileName)
  } else{
    t1 <- Sys.time()
    terr_out <-  gen_territories(riv_network, multicore = T, progbar = T)
    if (isTRUE(save_out)){
      saveRDS(terr_out, file=fileName)
    }
    message(sprintf('Potential Territory Generation Run Time = %s minutes', round(Sys.time()-t1,1)))
  }
  return(terr_out)
}
ORN_BeavNetOtterNEW <- ORN_BeavNetOtter
MMRN_BeavNetOtterNEW <-MMRN_BeavNetOtter

ORN_pot_terr <- run_terr_gen(ORN_BeavNetOtterNEW, overwrite = T)
MMRN_pot_terr <- run_terr_gen(MMRN_BeavNetOtterNEW, overwrite = T)


ggplot(MMRN_pot_terr, aes(x=Terr_Leng))+
  geom_density()

# v1 <- tibble(vals =  rnorm(1000, 1630, 293), func = 'rnorm')
# v2 <- tibble(vals =  runif(1000, min = 1630-293, max = 1630+293), func = 'runif')
# v3 <- bind_rows(v1, v2)
# ggplot(v3, aes(x=vals, fill=func)) +
#   geom_histogram()
# ------------- Run territory cap -------------
run_terr_cap <- function(pot_terrs, veg, overwrite=FALSE, save_out=TRUE){
  fileName <- file.path(export_dir, paste('TC_', deparse(substitute(pot_terrs)),'.Rds', sep=""))
  # print(fileName)
  if (file.exists(fileName) && isFALSE(overwrite)){
    message(sprintf("Territory capacity created in: %s", fileName))
    message("Loading previously generated file...")
    cap_out <- readRDS(fileName)
  } else {
    t1 <- Sys.time()
    cap_out <-territory_cap(pot_terrs, min_veg = veg , multicore = TRUE)
    if (isTRUE(save_out)){
      saveRDS(cap_out, file=fileName)
    }
    message(sprintf('Territory Capacity Run Time = %s minutes', round(Sys.time()-t1,1)))
  }
  return(cap_out)
}
ORN_pot_terrNEW <-ORN_pot_terr
MMRN_pot_terrNEW <- MMRN_pot_terr
ORN_terr_cap <- run_terr_cap(ORN_pot_terrNEW, 1.5, overwrite = T)
MMRN_terr_cap <- run_terr_cap(MMRN_pot_terr, 1.5)

# MMRN_pot_terr2_5 <- MMRN_pot_terr
# MMRN_terr_cap <- run_terr_cap(MMRN_pot_terr2_5, 2.5)
# ---------- Plot Territory Capacities for both Networks ---------

capacity_plot <- function(ORN_cap, MMRN_cap){
  p1 <- plot_capacity(ORN_cap, buffer=50, basemap = F, catchment = RivOtter_Catch_Area,
                      river_net = ORN_BeavNetOtter, plot_extent = target_ext, north_arrow = F,
                      scalebar = F, axes_units = F)
  p2 <- plot_capacity(MMRN_cap, buffer=50, basemap = F, catchment = RivOtter_Catch_Area,
                      river_net = MMRN_BeavNetOtter, plot_extent = target_ext, axes_units = F)
  p3 <- p1 + p2 + plot_annotation(
    caption = 'Contains: © OpenStreetMap contributors, \n
    OS data © Crown copyright and database right 2021'
  )
  return(p3)
}

capacity_plot(ORN_terr_cap, MMRN_terr_cap) %>%
  ggsave(filename = file.path(plot_dir, 'OSMM_terrs.jpg'),plot = ., dpi=300, height=7, width=10)

plot_capacity(MMRN_terr_cap, buffer=50, basemap = F, catchment = RivOtter_Catch_Area,
              river_net = MMRN_BeavNetOtter, plot_extent = target_ext, north_arrow = F,
              scalebar = F, axes_units = F) %>%
  ggsave(filename = file.path(plot_dir, 'OSMM_OLD.jpg'),plot = ., dpi=300, height=7, width=5)



sf::st_write(sf::st_buffer(test_TC_par, 50), 'QGIS/To_test/terr_sim_1kmCont.gpkg', driver=, "GPKG", append=FALSE, overwrite=T)



#------- Observed territory Habitat stats ----------
# need to calculate some stats here and find out what kind of BFI values we're currently seeing on the Otter.

plot_names <- unique(RivOtter_FeedSigns$SurveySeason)


Real_terrs %>%
  purrr::map2(.x=., .y=plot_names, ~ mutate(.x, SurveySeason = .y)) %>%
  bind_rows() %>%
  st_drop_geometry() %>%
  filter(user_class %in% c('Territory', 'Possibly'))

MMRN_terr_cap %>%
  ggplot(., aes(y=BFI_40m))+
  geom_boxplot()





