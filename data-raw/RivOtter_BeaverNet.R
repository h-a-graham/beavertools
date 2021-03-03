# Extract R. Otter BDC data from full national data.
# apologies - the national data is not provided in the package but hopefully we can make this available soon
# Method for creating the BDC data can be found in https://doi.org/10.1007/s10344-020-01379-w and the
# current repo can be found here: https://github.com/h-a-graham/BDC_V1.2
library(sf)
library(here)
library(tidyverse)
library(lubridate)
library(ggspatial)
devtools::load_all()

#national data:
BDC_nat <- file.path("D:/HG_Work/GB_Beaver_Data/OpenBeaverNetwork_GB_v0_1/OpenBeaverNetwork_GB_merge.gpkg")

BDC_nat_sf <- read_sf(BDC_nat)

# get intersection with Otter Catchment
RivOtter_BeaverNet <- st_intersection(BDC_nat_sf, RivOtter_Catch_Area) %>%
  mutate(BDC_cat = fct_relevel(BDC_cat, 'None', 'Rare', 'Occasional', 'Frequent' ,'Pervasive'))

# check intersection and plot.
ggplot(RivOtter_BeaverNet, aes(colour=BDC_cat)) +
  geom_sf() +
  scale_colour_manual(values=c('black','orange', 'yellow', 'green', 'blue'))

#export the dataset to the data folder.
usethis::use_data(RivOtter_BeaverNet, overwrite = TRUE)
