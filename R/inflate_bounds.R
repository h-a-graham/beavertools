#' A function to generate an inflated bbox for input to Kernel Density function
#'
#' generate a bounding box based on a sf/sp (readable) object
#'
#' @param sp_obj Either an sf object or an sf-readable file. See sf::st_drivers() for available drivers. used as the basis for the bounding box.
#' @param value A numeric distance (in the units of the spatial object) to inflate the bounds by.
#' @return object of class "bbox" containing the spatial coordinates of a bounding area
#' @export
#' @examples
#'
#' # generate inflated (by 10m) bounding box for ROBT feeding signs
#' inflate_bbox(RivOtter_FeedSigns, 10)
#'
inflate_bbox <- function(sp_obj, value){
  sp_obj <- check_spatial_feature(sp_obj, 'sp_obj')
  bounds <- sf::st_bbox(sp_obj)

  bounds[1] <- bounds[1] - value
  bounds[2] <- bounds[2] - value
  bounds[3] <- bounds[3] + value
  bounds[4] <- bounds[4] + value

  return(bounds)

}
