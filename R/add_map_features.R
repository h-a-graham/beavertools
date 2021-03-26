# function to create a feature with a hole in it equal to a provided catchment area
# help to highlight a particular region on a map
create_mask <- function(sf_obj){

    sf_obj %>%
    sf::st_buffer(2500) %>%
    sf::st_bbox()%>%
    sf::st_as_sfc() %>%
    sf::st_as_sf() %>%
    sf::st_difference(., sf_obj)
}

#' Retrieve an Open Street Map River Network for a given catchment area
#'
#' This function allows you to download a river network layer in 'sf' format for
#' a catchment area of your choice. This can be useful when making many plots of the
#' same area and saves frequent calls to the OSM server for each plot when using river = TRUE
#' in the `beavertools::terr_plot()` and `beavertools::plot_forage_density()` functions.
#'
#' @param catchment An sf object or an sf-readable file of the catchment area or area of interest.
#' See sf::st_drivers() for available drivers.
#' @export
#' @examples
#' # this generates an sf object for the river network of the R. Otter catchment.
#' get_rivers(RivOtter_Catch_Area)
#'
get_rivers <- function(catchment){
  catchment <- check_spatial_feature(catchment)

  catch_wgs <- catchment %>%
    sf::st_transform(crs = 4326)

  box <- catch_wgs %>%
    st_bbox()

  rivers <- osmdata::opq(bbox = box, memsize = 1073741824) %>%
    osmdata::add_osm_feature(key='waterway') %>%
    osmdata::osmdata_sf() %>%
    .$osm_lines %>%
    sf::st_intersection(catch_wgs) %>%
    sf::st_union()%>%
    sf::st_transform(crs = 4326) %>%
    sf::st_sf() %>%
    dplyr::mutate(river = 'River Network')
}


