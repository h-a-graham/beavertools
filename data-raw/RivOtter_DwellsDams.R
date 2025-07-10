# File to read in the ROBT dwelling and dam data recorded during annual surveys.
# These signs will be used as confirmatory signs to establish if areas of impact can be classified as territories.
# Data was super messy so has been collected and put into a single Geopackage.

library(sf)
library(here)
library(tidyverse)
library(lubridate)
library(ggspatial)


# --------- functions-----------

# Prep_sf_obj
prep_data <- function(.data, type, season) {
  .data %>%
    select(EDITTED, geom) %>%
    rename(RecordDate = EDITTED) %>%
    mutate(RecordDate = dmy_hms(RecordDate)) %>%
    mutate(SignType = type) %>%
    mutate(SurveySeason = season)
}


# ---------- load data -----------------

# pre 15
Dwell_Pre15 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_Pre15'
) %>%
  prep_data(., 'Dwelling', 'Pre 2015')

# Winter 15 - 16
Dwell_15_16 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_15_16'
) %>%
  prep_data(., 'Dwelling', '2015 - 2016')

Dam_15_16 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dam_15_16'
) %>%
  prep_data(., 'Dam', '2015 - 2016')

# Winter 16 -17
Dwell_16_17 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_16_17'
) %>%
  prep_data(., 'Dwelling', '2016 - 2017')

Dam_16_17 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dam_16_17'
) %>%
  prep_data(., 'Dam', '2016 - 2017')

# Winter 17 - 18
Dwell_17_18 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_17_18'
) %>%
  prep_data(., 'Dwelling', '2017 - 2018')

Dam_17_18 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dam_17_18'
) %>%
  prep_data(., 'Dam', '2017 - 2018')

# Winter 18 - 19
Dwell_18_19 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_18_19'
) %>%
  prep_data(., 'Dwelling', '2018 - 2019')

Dam_18_19 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dam_18_19'
) %>%
  prep_data(., 'Dam', '2018 - 2019')

# Witer 19 - 20
Dwell_19_20 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dwelling_19_20'
) %>%
  prep_data(., 'Dwelling', '2019 - 2020')

Dam_19_20 <- read_sf(
  'data-raw/Otter_survey_raw/Dam_Dwelling.gpkg',
  layer = 'Dam_19_20'
) %>%
  prep_data(., 'Dam', '2019 - 2020')


# Witer 19 - 20
Dwell_20_21 <- read_sf(file.path(
  here::here(),
  'data-raw',
  'Otter_survey_raw',
  'Otter_FS_20_21',
  'Dwellings16.SHP'
)) %>%
  rename(geom = geometry) %>%
  prep_data(., 'Dwelling', '2020 - 2021')

Dam_20_21 <- read_sf(file.path(
  here::here(),
  'data-raw',
  'Otter_survey_raw',
  'Otter_FS_20_21',
  'Dam16.SHP'
)) %>%
  rename(geom = geometry) %>%
  prep_data(., 'Dam', '2020 - 2021')


#  --------------- combine data ---------------------------------

RivOtter_OtherSigns <- list(
  Dwell_Pre15,
  Dwell_15_16,
  Dam_15_16,
  Dwell_16_17,
  Dam_16_17,
  Dwell_17_18,
  Dam_17_18,
  Dwell_18_19,
  Dam_18_19,
  Dwell_19_20,
  Dam_19_20,
  Dwell_20_21,
  Dam_20_21
) %>%
  purrr::map(~ sf::st_transform(.x, 27700)) %>%
  bind_rows() %>%
  mutate(SurveySeason = fct_relevel(SurveySeason, 'Pre 2015')) %>%
  mutate(SignType = fct_relevel(SignType, 'Dwelling')) %>%
  select(SurveySeason, RecordDate, SignType, geom)


# Check sf dataframe
ggplot() +
  ggspatial::annotation_map_tile(type = 'osm', zoomin = 0, alpha = 0.8) +
  layer_spatial(RivOtter_OtherSigns, aes(colour = SignType), alpha = 0.7) +
  coord_sf(
    crs = sf::st_crs(RivOtter_OtherSigns),
    datum = sf::st_crs(RivOtter_OtherSigns)
  ) +
  facet_wrap(~SurveySeason) +
  scale_colour_brewer(palette = 'Dark2') +
  theme_bw()


#export the dataset to the data folder.
usethis::use_data(RivOtter_OtherSigns, overwrite = TRUE)
