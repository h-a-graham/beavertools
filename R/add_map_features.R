# function to create a feature with a hole in it equal to a provided catchment area
# help to highlight a particular region on a map
create_mask <- function(sf_obj){

    sf_obj %>%
    sf::st_buffer(2000) %>%
    sf::st_bbox()%>%
    sf::st_as_sfc() %>%
    sf::st_as_sf() %>%
    sf::st_difference(., sf_obj)
}

# returns osm river network to be added to maps for additional info
get_rivers <- function(catchment){

  catch_wgs <- catchment %>%
    st_transform(crs = 4326)

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
    dplyr::mutate(river = 'River Network') %>%
    sf::st_transform(crs = 27700)
}


