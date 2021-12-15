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
#' @param guide Boolean - if TRUE then the legend is extracted from the list of figures and places below the panels
#' @param guide_fig_height numeric vector length 2 - giving the heights for the figure and lengend objects.
#' @param n_col integer denoting the number of columns the panel should use. if NULL (the default) the number is generated
#' automatically with `ceiling(sqrt(length(terr_plot_list)))`.
#' @returns TableGrob obect of a multi-panel map.
#' @export
#' @examples
#' #'# kde plots for panel/animation
#'fsd_ggplot <- function(.data, p.names, p.ext, add_map_stuff= FALSE){
#'
#'  if (isTRUE(add_map_stuff)){
#'    fsd <- plot_forage_density(.data, basemap = FALSE, guide = FALSE, catchment = RivOtter_Catch_Area,
#'                               rivers = FALSE, plot_extent = p.ext, axes_units = FALSE) +
#'      ggplot2::labs(subtitle = sprintf('Beaver Foraging Density: %s', p.names))
#'  } else {
#'    fsd <- plot_forage_density(.data, basemap = FALSE, axes_units = FALSE, north_arrow = FALSE, scalebar = FALSE, guide = FALSE,
#'                               catchment = RivOtter_Catch_Area, rivers = FALSE, plot_extent = p.ext) +
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
#'# generate panel plot showing sequence of feeding density maps
#' kde_panel <- kde_ras_list %>%
#'     purrr::map2(.x=., .y=plot_names, ~fsd_ggplot(.x, .y,
#'                 inflate_bbox(RivOtter_Catch_Area, 200))) %>%
#'     panel_plot(.)
#'
#'
panel_plot <- function(terr_plot_list, scalebar=TRUE, scalebar_loc = 'tl', north_arrow = TRUE,
                       north_arrow_loc = 'br', north_arrow_size = 0.5, guide = FALSE, guide_fig_height = c(30,1),
                       n_col=NULL){
  n <- length(terr_plot_list)

  #remove all legends from plots
  terr_plot_listNL <- terr_plot_list %>%
    purrr::map(., .f = function(x) x + ggplot2::guides(fill=F, colour=F, size=F))

  if (isTRUE(scalebar)){
    terr_plot_listNL[[n]] <- terr_plot_listNL[[n]] +
      ggspatial::annotation_scale(location= scalebar_loc)
  }

  if (isTRUE(north_arrow)){
    terr_plot_listNL[[n]] <- terr_plot_listNL[[n]] +
      ggspatial::annotation_north_arrow(location = north_arrow_loc, which_north = "true",
                                        height = ggplot2::unit(north_arrow_size, "cm"),
                                        width = ggplot2::unit(north_arrow_size, "cm"),
                                        style = ggspatial::north_arrow_orienteering(text_col=NA,
                                                                                    fill = c("black", "black")))
  }

  if (!is.null(n_col)){
    nCol = n_col
  } else {
    nCol <- ceiling(sqrt(n))
  }

  p <- do.call(gridExtra::grid.arrange, c(terr_plot_listNL, ncol=nCol))

  #extract legend
  if (isTRUE(guide)){
    leg <- ggpubr::get_legend(terr_plot_list, position = "bottom") %>%
      ggpubr::as_ggplot()
    p <- gridExtra::grid.arrange(p, leg, ncol=1, heights=c(guide_fig_height[1], guide_fig_height[2]),
                                 bottom=grid::textGrob("© OpenStreetMap contributors",
                                                       gp=grid::gpar(fontsize=6)))
  }

  return(p)
}
