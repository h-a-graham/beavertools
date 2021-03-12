# 2021 feeding sign data cleaning.
# this year's data is a slightly different format - this just pulls it together...

robt_20_21_FSclean <- function(){
  root_folder <- file.path(here::here(), 'data-raw', 'Otter_survey_raw')
  raw_folder <- file.path(here::here(), 'data-raw', 'Otter_survey_raw', 'Otter_FS_20_21')

  f_names <- list('Low-Level Impact.SHP', 'Medium-Level Impact.SHP', 'High-Level Impact.SHP')
  impacts <- list('Low', 'Med', 'High')

  read_shp <- function(shp, imp_cat){
    file.path(raw_folder, shp) %>%
      sf::read_sf(.) %>%
      dplyr::mutate(imp_level=imp_cat)
  }

  out_df <- f_names %>%
    purrr::map2(.x=., .y = impacts, ~read_shp(.x, .y)) %>%
    dplyr::bind_rows() %>%
    sf::write_sf(., file.path(root_folder, 'SysSurv_FS_2021.shp'))

}

