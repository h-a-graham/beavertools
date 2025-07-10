check_spatial_feature <- function(.feature, .name) {
  # check the input and convert to sf from either filepath or sp. skip if already sf.
  if (class(.feature)[1] == "sf") {
    # forage objects already in sf format
  } else if (class(.feature)[1] == "SpatialPointsDataFrame") {
    .feature <- (sf::st_as_sf(.feature))
  } else if (class(.feature)[1] == 'character') {
    .feature <- sf::read_sf(normalizePath(file.path(.feature)))
  } else {
    stop(sprintf(
      'Class type %s is not supported for "%s" arg.
         Please provide either: sf, SpatialPointsDataFrame, or sf-readable filepath',
      class(.feature)[1],
      .name
    ))
  }

  return(.feature)
}
