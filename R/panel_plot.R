#' generate a panel plot of multiple maps generated with `plot_territories` or `plot_forage_density`
#'
#'
#' @export
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
