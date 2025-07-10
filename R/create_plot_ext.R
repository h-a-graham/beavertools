create_plot_ext <- function(p_ext) {
  if (class(p_ext)[1] == "bbox") {
    p_ext <- sf::st_sf(geometry = sf::st_as_sfc(p_ext)) %>%
      sf::st_transform(crs = 4326) %>%
      define_extent_sp(.)
  } else if (
    class(p_ext)[1] == "sf" ||
      class(p_ext)[1] == "SpatialPointsDataFrame" ||
      class(p_ext)[1] == "raster"
  ) {
    p_ext <- p_ext %>%
      sf::st_transform(crs = 4326) %>%
      define_extent_sp(.)
  } else if (class(p_ext)[1] == 'character') {
    p_ext <- sf::read_sf(p_ext) %>%
      sf::st_transform(crs = 4326) %>%
      define_extent_sp(.)
  } else {
    stop(sprintf(
      "Class ' %s ' is not a valid extent input use either: 'bbox', 'sf' or 'sp'",
      class(p_ext)[1]
    ))
  }
  return(p_ext)
}
