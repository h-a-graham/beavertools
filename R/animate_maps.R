
#' Function to Animate a list of maps generated with either `plot_territories` or `plot_forage_density`
#'
#' This function generates an animation between map time steps to visualise change over time using
#' the {magick} package.
#'
#' @param map_list A list of ggplot objects generated with either  `plot_territories` or `plot_forage_density`
#' @param filename character string for the save path if saving to disk is required.
#' @param x_pix numeric denoting width of the animation canvas in pixels
#' @param y_pix numeric denoting height of the animation canvas in pixels
#' @param pix_res numeric denoting canvas resolution
#' @param n_frames numeric for the number of frames to use for transitions
#' @param fps numeric for the number of frames per second for the resulting animation
#' @return Animated map of esither territory locations over time or feeding density over time.
#' @export
#' @examples
#'
#'# kde plots for panel/animation
#'fsd_ggplot <- function(.data, p.names, p.ext, add_map_stuff= FALSE){
#'
#'  if (isTRUE(add_map_stuff)){
#'    fsd <- plot_forage_density(.data, guide = FALSE, catchment = RivOtter_Catch_Area,
#'                               rivers = TRUE, plot_extent = p.ext, axes_units = FALSE) +
#'      ggplot2::labs(subtitle = sprintf('Beaver Foraging Density: %s', p.names))
#'  } else {
#'    fsd <- plot_forage_density(.data, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE, guide = FALSE,
#'                               catchment = RivOtter_Catch_Area, rivers = TRUE, plot_extent = p.ext) +
#'      ggplot2::labs(subtitle = p.names)
#'  }
#'
#'  return(fsd)
#'}
#'
#' # get name list for plots
#' plot_names <- unique(RivOtter_FeedSigns$SurveySeason)
#'
# get constant extent for all plots
#' ras_ext <- inflate_bbox(RivOtter_FeedSigns, 250)
#'
#'# generate KDE rasters for all survey periods
#' kde_ras_list <- RivOtter_FeedSigns %>%
#'   dplyr::group_by(SurveySeason) %>%
#'   dplyr::group_map(., ~forage_density(., 'FeedCat', kd_extent = ras_ext))
#'
#'# animate kernel density rasters
#' kde_animation <- kde_ras_list %>%
#' purrr::map2(.x=., .y=plot_names, ~fsd_ggplot(.x, .y, inflate_bbox(RivOtter_Catch_Area, 200),
#'   add_map_stuff = TRUE)) %>%
#'   animate_maps(.)
animate_maps <- function(map_list, filename=NULL, x_pix=1200, y_pix=675, pix_res=96, n_frames=10, fps = 5){

  img <- magick::image_graph(x_pix, y_pix, res = pix_res)
  print(map_list)
  dev.off()
  animation <- magick::image_animate(magick::image_morph(img,  frames = n_frames), fps = fps, optimize = TRUE)

  if (!is.null(filename)){
    magick::image_write(animation, normalizePath(file.path(filename), mustWork = FALSE))
  }

  return(animation)

}
