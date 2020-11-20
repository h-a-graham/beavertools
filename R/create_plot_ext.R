create_plot_ext <- function(p_ext){
  if (class(p_ext)[1] == "bbox"){
    p_ext <- define_extent_bbox(p_ext, 1)

  } else if (class(p_ext)[1] == "sf" || class(p_ext)[1] == "SpatialPointsDataFrame" || class(p_ext)[1] == "raster"){
    p_ext <- define_extent_sp(p_ext, 1)

  } else if (class(p_ext)[1] == 'character'){
    p_ext <- sf::read_sf(p_ext)
    p_ext <- define_extent_sp(p_ext, 1)

  } else{
    stop(sprintf("Class ' %s ' is not a valid extent input use either: 'bbox', 'sf' or 'sp'", class(p_ext)[1]))
  }
  return(p_ext)
}
