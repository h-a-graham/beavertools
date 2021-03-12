#Generate R. Otter Survey sf object. data is from annual winter feeding sign surveys of Beaver impacts
# during the River Otter Beaver Trial (ROBT)

library(sf)
library(here)
library(tidyverse)
library(lubridate)
library(ggspatial)

# load and run function to clean up the 20/21 data
source(file.path(here(), 'data-raw', 'robt_20_21_clean.R'))
robt_20_21_FSclean()


# Collect file names for various annual winter feeding surveys from ROBT
survey_shp_paths <- as.list(list.files('data-raw/Otter_survey_raw', pattern = '*.shp'))
survey_shp_paths <- purrr::discard(survey_shp_paths , grepl(".shp.xml", survey_shp_paths ))

# function to read and clean the various .shp files.
read_features <- function(rel.path){
  sf.obj <- file.path(here(), 'data-raw/Otter_survey_raw', rel.path) %>%
    read_sf()

  if ('imp_level' %in% colnames(sf.obj)){
    sf.obj <- sf.obj %>%
      mutate(FeedCat = imp_level)
  } else if ('Imp_Cat' %in% colnames(sf.obj)){
    sf.obj <- sf.obj %>%
      mutate(FeedCat = Imp_Cat)
  } else if ('weight' %in% colnames(sf.obj)){
    sf.obj <- sf.obj %>%
      mutate(FeedCat = ifelse(weight == 10, 'High',
                              ifelse(weight == 4, 'Med',
                                     ifelse(weight == 1, 'Low',
                                            NA))))
  }

  sf.obj <- sf.obj %>%
    select(EDITTED, FeedCat, geometry) %>%
    drop_na() %>%
    rename(RecordDate = EDITTED)%>%
    mutate(RecordDate = dmy_hms(RecordDate)) %>%
    st_as_sf()
  return(sf.obj)

}

# function to add a custom season value.
add_season <- function(.data, season){
  .data %>%
    mutate(SurveySeason = season)
}

# map read function to files
OtterFS_list <- purrr::map(survey_shp_paths, .f=read_features)

# add custom season value to each dataset
Ott_FSS_pre15 <- OtterFS_list[[5]] %>% add_season(., 'Pre 2015')
Ott_FSS_15_16 <- OtterFS_list[[1]] %>% add_season(., '2015 - 2016')
Ott_FSS_16_17 <- OtterFS_list[[2]] %>% add_season(., '2016 - 2017')
Ott_FSS_17_18 <- OtterFS_list[[3]] %>% add_season(., '2017 - 2018')
Ott_FSS_18_19 <- OtterFS_list[[4]] %>% add_season(., '2018 - 2019')
Ott_FSS_19_20 <- OtterFS_list[[6]] %>% add_season(., '2019 - 2020')
Ott_FSS_20_21 <- OtterFS_list[[7]] %>% add_season(., '2020 - 2021')
#bind all seasons together to single tibble
RivOtter_FeedSigns <- Ott_FSS_pre15 %>%
  bind_rows(Ott_FSS_15_16, Ott_FSS_16_17, Ott_FSS_17_18, Ott_FSS_18_19, Ott_FSS_19_20, Ott_FSS_20_21) %>%
  mutate(FeedCat = ifelse(FeedCat == 'low', 'Low', FeedCat)) %>%
  mutate(FeedCat = ifelse(FeedCat == 'high', 'High', FeedCat)) %>%
  mutate(FeedCat = ifelse(FeedCat == 'med', 'Med', FeedCat)) %>%
  mutate(FeedCat = fct_relevel(FeedCat, 'Low', 'Med', 'High')) %>%
  mutate(SurveySeason = fct_relevel(SurveySeason, 'Pre 2015', '2015 - 2016',
                                    '2016 - 2017', '2017 - 2018', '2018 - 2019',
                                    '2019 - 2020', '2020 - 2021')) %>%
  select(SurveySeason, RecordDate, FeedCat, geometry)


# Plot to see if data looks sensible
ggplot(RivOtter_FeedSigns, aes(colour=FeedCat)) +
  annotation_map_tile(type = "cartolight", zoomin = -1)+
  geom_sf(alpha = 0.3) +
  scale_colour_brewer(type = 'qual', palette = 'Set1', direction = -1) +
  coord_sf(crs = 27700, datum = sf::st_crs(27700)) +
  theme_bw() +
  facet_wrap(~SurveySeason)


ggplot(RivOtter_FeedSigns) +
  annotation_map_tile(type = "cartolight", zoomin = -1)+
  geom_sf(colour='blue', alpha = 0.3) +
  coord_sf(crs = 27700, datum = sf::st_crs(27700)) +
  theme_bw()

# bar plot to check out if n for categories looks sound.
ggplot(RivOtter_FeedSigns, aes(x = FeedCat, fill=FeedCat)) +
  geom_bar(alpha = 0.6) +
  scale_fill_brewer(type = 'qual', palette = 'Set1', direction = -1) +
  theme_bw() +
  facet_wrap(~SurveySeason)

#export the dataset to the data folder.
usethis::use_data(RivOtter_FeedSigns, overwrite = TRUE)
