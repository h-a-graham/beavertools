
#' Estimate the number of territories in a catchment
#'
#' Uses the kernel density maps to estimate the area of territories by locating areas of activity that
#' contain foraging intensity above a given threshold, which locates potential territories - to confirm
#' territories, confirmatory signs are required including dam and dwelling locations.
#'
#' @param forage_raster the foraging density raster generated from `beavertools::forage_density()`
#' @param confirm_signs An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' containing 'confirmatory' field signs for beaver such as dams and lodges. This is required to assign a
#' confidence classification for areas of activity either: 'Activity'. 'Possible' or 'Territory'.
#' @param low_thresh numeric between 0 and 1. Used to set minimum value for activity. Lower values
#' return more information i.e. area with cover but can result in the merging of territories as density
#' increases. use lowest possible value.
#' @param upper_thresh numeric between 0 and 1. Used to set the minimum value for core areas of activity. AS
#' central place forages, beavers feed more near their lodge, this value idenitfies areas of activity which may
#' indicate the centre of a territory. default is 0.95, may need adjusting depending on territory density.
#' @return An 'sf' polygon object defining areas of beaver activity, with kernel denisty summary stats and
#' the territory status.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#'
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#'# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
#'# year as the forage density raster.
#'
#' CS_201920 <- RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")
#'
#'# run territory classification
#' otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)
#'
estimate_territories <- function(forage_raster, confirm_signs, low_thresh = 0, upper_thresh = 0.95){

  # check the input confirm points and convert to sf from either filepath or sp. skip if already sf.
  if (class(confirm_signs)[1] == "sf") {
    # forage objects already in sf format

  } else if (class(confirm_signs)[1] == "SpatialPointsDataFrame") {
    confirm_signs <- (sf::st_as_sf(confirm_signs))

  } else if (class(confirm_signs)[1] == 'character'){
    confirm_signs <- sf::read_sf(confirm_signs)

  } else {
    stop('Class type %s is not supported for "confirm_signs" arg.
         Please provide either: sf, SpatialPointsDataFrame, or sf-readable filepath')
  }

  # silence warnings
  oldw <- getOption("warn")
  options(warn = -1)

  poly_list <- list(low_thresh, upper_thresh) %>%
    purrr::map(., ~ raster::quantile(forage_raster, .)) %>%
    purrr::map(., ~ polygonise(forage_raster, .))

  # restore warning setting
  options(warn = oldw)

  forage_poly <- poly_list[[1]] %>%
    dplyr::mutate(id = forcats::as_factor(dplyr::row_number())) %>%
    dplyr::mutate(Upper_Thresh = ifelse(id %in% unlist(st_intersects(poly_list[[2]], poly_list[[1]])),
                                        'Yes', 'No')) %>%
    dplyr::mutate(Confirm_signs = ifelse(id %in% unlist(st_intersects(confirm_signs, poly_list[[1]])),
                                         'Yes', 'No')) %>%
    dplyr::mutate(terr_status = ifelse(Upper_Thresh=='Yes' & Confirm_signs=='Yes', 'Territory',
                                       ifelse(Upper_Thresh=='Yes' & Confirm_signs=='No', 'Possible',
                                              ifelse(Upper_Thresh=='No' & Confirm_signs=='Yes', 'Possible',
                                                     'Activity')))) %>%
    dplyr::mutate(mean_fd = exactextractr::exact_extract(forage_raster, ., 'mean', progress =F))%>%
    dplyr::mutate(sum_fd = exactextractr::exact_extract(forage_raster, ., 'sum', progress =F)) %>%
    dplyr::rename(geometry = x)


  return(forage_poly)
}

# function to convert raster to polygons
polygonise <- function(ras, thresh){

  rasvec <- as.vector(ras)
  rasvec <- rasvec[!is.na(rasvec)]
  f <- ecdf(rasvec)
  perc <- round(f(thresh), 2)

  raster::rasterToPolygons(ras, fun=function(x){x>thresh}) %>%
    sf::st_as_sf() %>%
    sf::st_union() %>%
    sf::st_as_sf() %>%
    dplyr::mutate(quant = perc) %>%
    dplyr::mutate(quantf = as.factor(quant)) %>%
    sf::st_cast(., to='POLYGON')
}



#' Add user specified classification of territory areas
#'
#' Opportunity to add a new column 'user_class' which enables the user to confirm or reject
#' the automated assessment of territory areas.
#'
#' @param territory_poly a territory polygon generated with `beavertools::estimate_territories()`
#' @param territory  numeric vector containing the ID numbers for areas to be reclassified as 'Territory'. e.g. c(10, 28)
#' @param possible numeric vector containing the ID numbers for areas to be reclassified as 'Possible'
#' @param activity numeric vector containing the ID numbers for areas to be reclassified as 'Activity'
#' @return territory_poly is returned with the an additional column 'user_class'
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#'# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
#'# year as the forage density raster.
#'
#' CS_201920 <- RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")
#'
#'# run territory classification
#' otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)
#'
#'# create the map for checking automated territory classification
#' check_auto_terr(otter_poly, basemap=FALSE, label=TRUE)
#'
#' otter_poly_uc <- user_classify(otter_poly, territory = c(10, 28))
user_classify <- function(territory_poly, territory=NULL, possible=NULL, activity=NULL){

  class_overlap <- territory %in% possible

  if (isTRUE(TRUE %in% class_overlap)){
    stop('A territory is duplicated in both possible and territory lists')
  }

  territory_poly %>%
    dplyr::mutate(user_class = terr_status) %>%
    dplyr::mutate(user_class = ifelse(id %in% territory, 'Territory',
                                      ifelse(id %in% possible, 'Possible',
                                             ifelse(id %in% activity, 'Activity',
                                                    terr_status)))) %>%
    dplyr::relocate(user_class, .after = terr_status)


}

