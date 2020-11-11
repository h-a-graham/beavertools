#' Function to generate Kernel Density plot from Beaver Forage Data
#'
#' This function allows users to generate a kernel density raster from a collection of points.
#'
#' @export
forage_density <- function(forage_points, impact_cat, grid_size = 20, kern_bw = 250, kd_extent,
                           kd_weights = c(1, 1e+03, 1e+06), low_thresh = 1e-12){

  # check the input forage points and convert to sf from either filepath or sp. skip if already sf.
  if (class(forage_points)[1] == "sf") {
    # forage objects already in sf format

  } else if (class(forage_points)[1] == "SpatialPointsDataFrame") {
    forage_points <- (sf::st_as_sf(forage_points))

  } else if (class(forage_points)[1] == 'character'){
    forage_points <- sf::read_sf(forage_points)

  } else {
    stop('Class type %s is not supported for "forage_points" arg.
         Please provide either: sf, SpatialPointsDataFrame, or sf-readable filepath')
  }

  if (is.na(sf::st_crs(forage_points))) {
    stop("A valid CRS is missing in 'forage_points'")
  }

  # add weight values to sf object
  if (missing(impact_cat)){
    message('WARNING: No value supplied for "impact_cat" argument. Kernel density estimate will not be weighted.')
    weighted_kde <- FALSE

  } else {

    forage_points <- forage_points %>%
      dplyr::mutate(weights = ifelse(!! dplyr::sym(impact_cat) == 'Low', kd_weights[1],
             ifelse(!! dplyr::sym(impact_cat) == 'Med', kd_weights[2],
                    ifelse(!! dplyr::sym(impact_cat) == 'High', kd_weights[3],
                           NA))))
    weighted_kde <- TRUE
  }

  #convert back to an sp SpatialPointsDataFrame (the required class for the kernel density estimate in {spatialEco})
  forage_points <- sf::as_Spatial(forage_points)


  # define the kenerl density extent
  if (missing(kd_extent)){
    message('No value supplied for "kd_extent" argument: default extent will be used')

    dist_offset <- plyr::round_any(kern_bw, grid_size, f = ceiling)

    kd_extent <- define_extent_sp(forage_points, dist_offset)

  } else if (class(kd_extent)[1] == "bbox"){
    kd_extent <- define_extent_bbox(kd_extent, grid_size)

  } else if (class(kd_extent)[1] == "sf" || class(kd_extent)[1] == "SpatialPointsDataFrame" || class(kd_extent)[1] == "raster"){
    kd_extent <- define_extent_sp(kd_extent, grid_size)

  } else if (class(kd_extent)[1] == 'character'){
    kd_extent <- sf::read_sf(kd_extent)
    kd_extent <- define_extent_sp(kd_extent, grid_size)

  } else{
    stop(sprintf("Class ' %s ' is not a valid extent input use either: 'bbox', 'sf' or 'sp'", class(kd_extent)[1]))
  }

  # returns number of rows and columns required for the raster
  dims <- set_dims(grid_size, kd_extent)

  # Run the kernel Density Estimate
  if (isTRUE(weighted_kde)){
  KernDen <- spatialEco::sp.kde(forage_points, y=forage_points$weights, bw=kern_bw, nr = dims$nrows[1], nc = dims$ncols[1], newdata = kd_extent,
                                standardize=FALSE)
  } else {
    KernDen <- spatialEco::sp.kde(forage_points, bw=kern_bw, nr = dims$nrows[1], nc = dims$ncols[1], newdata = kd_extent,
                                  standardize=FALSE)
  }


  # Set all 0 values to NA and remove low values < the low threshold.
  KernDen[KernDen == 0] <- NA
  KernDen[KernDen < low_thresh] <- NA

  return(KernDen)

}



# function to create appropriate bounds for bbox object
define_extent_bbox <- function(sf_bbox, offset) {

  xmin_ <- plyr::round_any(sf_bbox[[1]] - offset, offset, f = floor)
  ymin_ <- plyr::round_any(sf_bbox[[2]] - offset, offset, f = floor)
  xmax_ <- plyr::round_any(sf_bbox[[3]] + offset, offset, f = ceiling)
  ymax_ <- plyr::round_any(sf_bbox[[4]] + offset, offset, f = ceiling)

  return(c(xmin_, xmax_, ymin_, ymax_))

}

# function to create appropriate bounds for sp, sf or raster object
define_extent_sp <- function(sp_obj, offset){
  bounds <- sf::st_bbox(sp_obj)
  return(define_extent_bbox(bounds, offset))

}


# function to set the grid size.
set_dims <- function(grid_res, grid_ext){

  ncols <- (grid_ext[2] - grid_ext[1])/grid_res

  nrows <- (grid_ext[4] - grid_ext[3])/grid_res

  return(list(ncols=ncols, nrows=nrows))

}

