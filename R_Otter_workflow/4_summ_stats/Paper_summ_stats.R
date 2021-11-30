# quick summary stats for the paper.
library(beavertools)
library(tidyverse)
library(sf)
library(broom)
library(patchwork)
library(here)

# n feeding signs:
n_signs <- RivOtter_FeedSigns %>%
  filter(SurveySeason!= "Pre 2015") %>%
  mutate(year = ifelse(SurveySeason=='2015 - 2016' , 2016,
                              ifelse(SurveySeason== '2016 - 2017', 2017,
                                     ifelse(SurveySeason== '2017 - 2018', 2018,
                                            ifelse(SurveySeason== '2018 - 2019', 2019,
                                                   ifelse(SurveySeason== '2019 - 2020', 2020,
                                                          2021)))))) %>%
  group_by(year, FeedCat)%>%
  summarise(n =n()) %>%
  mutate(FeedCat = ifelse(FeedCat=='Low', 'Low effort',
                          ifelse(FeedCat=='Med', 'Medium effort',
                                 'High effort'))) %>%
  mutate(FeedCat = fct_relevel(FeedCat, 'Low effort', 'Medium effort', 'High effort'))

n_signs_c <- n_signs %>%
  summarise(n =sum(n)) %>%
  mutate(add_lab = 'Total feeding sign count')

p_func <- function(df, xlab = '', ylab='', html_col = '#8DBCE5'){
  ggplot(df, aes(x=year, y= n)) +
    geom_point()+
    geom_smooth(method = 'glm', method.args = list(family = gaussian(link = 'log')), colour=html_col) +
    labs(x=xlab, y = ylab) +
    theme_bw()
}


p1 <- p_func(n_signs, ylab='n recorded feeding signs') +
  facet_wrap(~FeedCat, scales='free')


p2 <- p_func(n_signs_c, xlab='year', html_col = '#4074E5')+
  facet_wrap(~add_lab)

p3 <- p1/p2

ggsave(filename = file.path(here(), 'R_Otter_workflow/4_summ_stats/plots/feeding_trends.png'), plot=p3,
         dpi=300, height=152, width=180, units='mm')

# territory counts.

terr_list <- readRDS(file=file.path(here::here(),'R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list2.Rds'))
survey_years <- RivOtter_FeedSigns %>%
  filter(SurveySeason!= "Pre 2015") %>%
  pull(SurveySeason) %>% unique()

date_list <- lubridate::dmy(c("30-12-2015", "30-12-2016",
                              "30-12-2017", "30-12-2018", "30-12-2019",
                              "30-12-2020"))
years_since_release <- c(9:14)

get_terr_counts <- function(terr_map, .season, .year, .ysr){
  terr_map %>%
    dplyr::filter(user_class == 'Territory') %>%
    summarise(n = dplyr::n(), season= .season, year = .year, years_since = .ysr)

}

count_obj_list <- list(terr_list, survey_years, date_list, years_since_release)
terr_counts <-  purrr::pmap(count_obj_list, ~get_terr_counts(..1, ..2, ..3, ..4)) %>%
  bind_rows() %>%
  st_drop_geometry() %>%
  tibble() %>%
  mutate(year_adj = years_since + 2007) %>%
  mutate(n = ifelse(season %in% c('2018 - 2019', '2019 - 2020'), n+1,
                             ifelse(season %in% c('2020 - 2021'),n + 2, n))) %>% # required because some territories not correctly identified due to semi-automate process.
  mutate(name = 'Territory counts')

p4 <- p_func(terr_counts, xlab='year', html_col = '#3DC660') +
  facet_wrap(~name)

p5 <- p1/ (p2 | p4)

ggsave(filename = file.path(here(), 'R_Otter_workflow/4_summ_stats/plots/feeding_Terr_trends.png'), plot=p5,
       dpi=300, height=152, width=180, units='mm')


# calc densities...

MMRN_BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg') # MasterMap Data

chan_leng <- as.numeric(sum(sf::st_length(MMRN_BeavNetOtter)))/1000

message(sprintf('areal density range is: %s - %s', round(120/chan_leng,2), round(183/chan_leng, 2)))

