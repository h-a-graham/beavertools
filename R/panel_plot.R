#' generate a panel plot of multiple maps generated with `plot_territories` or `plot_forage_density`
#'
#' Function to help build a panel plot consisting of multiple plots from different time periods.
#'
#' @param terr_plot_list list of ggplots generated from either `plot_territories` or `plot_forage_density`
#' @param scalebar Boolean to add a scalebar to the last plot in the sequence
#' @param scalebar_loc character vector to determine the scalebar location on final plot:
#' 'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to add a north arrow to the last plot in the sequence
#' @param north_arrow_loc character vector to determine the north arrow location on final plot:
#' 'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric to set the size of the north arrow
#' @returns TableGrob obect of a multi-panel map.
#' @export
#' @examples
#' #'# kde plots for panel/animation
#'fsd_ggplot <- function(.data, p.names, p.ext, add_map_stuff= FALSE){
#'
#'  if (isTRUE(add_map_stuff)){
#'    fsd <- plot_forage_density(.data, basemap = FALSE, guide = FALSE, catchment = Otter_catch,
#'                               rivers = FALSE, plot_extent = p.ext, axes_units = FALSE) +
#'      labs(subtitle = sprintf('Beaver Foraging Density: %s', p.names))
#'  } else {
#'    fsd <- plot_forage_density(.data, basemap = FALSE, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE, guide = FALSE,
#'                               catchment = Otter_catch, rivers = FALSE, plot_extent = p.ext) +
#'      labs(subtitle = p.names)
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
#'   group_by(SurveySeason) %>%
#'   group_split()%>%
#'   purrr::map(., ~forage_density(., 'FeedCat', kd_extent = ras_ext))
#'
#'# generate panel plot showing sequence of feeding density maps
#' kde_panel <- kde_ras_list %>%
#'     purrr::map2(.x=., .y=plot_names, ~fsd_ggplot(.x, .y, target_ext)) %>%
#'     panel_plot(.)
#'
#'# save to disk if you like...
#'# ggsave('panel_plot_map.png',plot = kde_panel)
#'
panel_plot <- function(terr_plot_list, scalebar=TRUE, scalebar_loc = 'tl',
                                 north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.5){
  n <- length(terr_plot_list)

  if (isTRUE(scalebar)){
    terr_plot_list[[n]] <- terr_plot_list[[n]] +
      ggspatial::annotation_scale(location= scalebar_loc)
  }

  if (isTRUE(north_arrow)){
    terr_plot_list[[n]] <- terr_plot_list[[n]] +
      ggspatial::annotation_north_arrow(location = north_arrow_loc, which_north = "true",
                                        height = ggplot2::unit(north_arrow_size, "cm"),
                                        width = ggplot2::unit(north_arrow_size, "cm"),
                                        style = ggspatial::north_arrow_orienteering(text_col=NA,
                                                                                    fill = c("black", "black")))
  }


  nCol <- ceiling(sqrt(n))
  do.call(gridExtra::grid.arrange, c(terr_plot_list, ncol=nCol))
}
