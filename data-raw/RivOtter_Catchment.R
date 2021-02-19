# Generate R. Otter Catchment sf object....

library(sf)
library(here)
library(magrittr)
library(ggplot2)
library(dplyr)

# Collect file names for various annual winter feeding surveys from ROBT
RivOtter_Catch_Area <- file.path(here(), 'data-raw/catchment_area', 'Otter_Catch_fix.gpkg') %>%
  sf::read_sf(.) %>%
  mutate(Name = 'River Otter') %>%
  select(Name, geom)


ggplot(RivOtter_Catch_Area) +
  geom_sf() +
  coord_sf(crs = 27700, datum = sf::st_crs(27700))

usethis::use_data(RivOtter_Catch_Area, overwrite = TRUE)
