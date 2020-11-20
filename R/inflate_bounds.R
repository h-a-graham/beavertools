#' A function to provide an inflated bbox for input to Kernel Density function
#'
#'
#' @export
inflate_bbox <- function(sp_obj, value){
  sp_obj <- check_spatial_feature(sp_obj, 'sp_obj')
  bounds <- sf::st_bbox(sp_obj)

  bounds[1] <- bounds[1] - value
  bounds[2] <- bounds[2] - value
  bounds[3] <- bounds[3] + value
  bounds[4] <- bounds[4] + value

  return(bounds)

}
