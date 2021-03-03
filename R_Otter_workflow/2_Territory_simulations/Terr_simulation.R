# ------------ imports --------------------
devtools::install()
# devtools::document()
library(tidyverse)
library(sf)
library(broom)
library(patchwork)
devtools::load_all()
# ----- generate extent and titles for mapping ------
target_ext <- inflate_bbox(RivOtter_Catch_Area, 10)
# ------------ Read Data ------------------
# This is the high res version of the Beaver Network and cannot be released with the package due to
# licensing issues. When we produce the OPen BeaverNetwork the network for the R. Otter will be included as
# a built in dataset.

BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg')

# --------
p1 <- BeavNetOtter %>%
  filter(Drain_Area > 1) %>%
  ggplot(., aes(colour=as.factor(Str_order)))+
    geom_sf()+
  scale_colour_brewer(palette = 'Dark2') +
  guides(colour=F)

p2 <- BeavNetOtter %>%
  filter(Drain_Area > 0.5) %>%
  ggplot(., aes(colour=as.factor(Str_order)))+
  geom_sf()+
  scale_colour_brewer(palette = 'Dark2') +
  guides(colour=F)

p3 <- BeavNetOtter %>%
  ggplot(., aes(colour=as.factor(Str_order)))+
  geom_sf()+
  scale_colour_brewer(palette = 'Dark2')


p1 + p2 + p3





# ---------- Subset dataset for now - > 1st order streams only -----------

# BeavNetOtter <- BeavNetOtter[BeavNetOtter$Str_order > 1,]

# BeavNetOtter <- BeavNetOtter[BeavNetOtter$Str_order > 3,]

BeavNetOtter <- BeavNetOtter%>%
  filter(Drain_Area > 1)

# ---------- run terriroty generation for all reaches
t1 <- Sys.time()
test_out <-  gen_territories(BeavNetOtter)
Sys.time()-t1

# sf::st_write(sf::st_buffer(test_out, 10), 'QGIS/To_test/All_terr_3.gpkg', driver=, "GPKG", append=FALSE, overwrite=T)

# ------------- Run territory cap -------------
t1 <- Sys.time()
test_TC_par <-territory_cap(test_out, min_veg = 2.5 , multicore = TRUE)
Sys.time()-t1
sf::st_write(sf::st_buffer(test_TC_par, 50), 'QGIS/To_test/terr_sim_1kmCont.gpkg', driver=, "GPKG", append=FALSE, overwrite=T)
plot_capacity(test_TC_par, buffer=50, basemap = F, catchment = RivOtter_Catch_Area, river_net = BeavNetOtter, plot_extent = target_ext)
