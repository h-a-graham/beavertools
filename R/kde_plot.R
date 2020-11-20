# ' Built in plotting function for Kernel density Raster.
#' @export
plot_forage_density <- function(kd_raster, basemap=TRUE, basemap_type = "osmgrayscale", trans_fill = TRUE, trans_type = 'log10',
                     axes_units = TRUE, scalebar=TRUE, scalebar_loc = 'tl', north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                     wsg=FALSE, guide=TRUE, catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL){


  # define extent
  set_lims <- TRUE

  if (is.null(plot_extent)){
    set_lims <- FALSE
  } else {
    plot_extent <- create_plot_ext(plot_extent)
  }

  p <- ggplot2::ggplot()

  if (isTRUE(basemap)){
    p <- p + ggspatial::annotation_map_tile(type = basemap_type, zoomin = 0, alpha = 0.8)
  }

  if (isTRUE(add_hillshade)){
    p <- p + ggspatial::annotation_map_tile(type = 'hillshade', zoomin = 0)
  }

  if (!is.null(catchment)){
    catchment <- check_spatial_feature(catchment, 'catchment')
    catch_mask <- create_mask(catchment)
    p <- p + ggspatial::annotation_spatial(catch_mask, fill = "grey50", alpha=0.5)
  }

  if (isTRUE(rivers)){
    if (is.null(catchment)) {
      message('"rivers" argument ignored. A valid "catchment" area is required.')
    } else {
      river_sf <- get_rivers(catchment)
      p <- p + ggspatial::annotation_spatial(river_sf, colour = "#5699FA", alpha=0.9, size=0.2)
    }

  }

  p <- p + ggspatial::layer_spatial(kd_raster, ggspatial::aes(fill=stat(band1)),alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_text(size=10))


  if (isTRUE(trans_fill)){
    p <- p + ggplot2::scale_fill_viridis_c(na.value = NA, name= sprintf('%s Forage Density', trans_type), trans=trans_type)
  } else {
    p <- p + ggplot2::scale_fill_viridis_c(na.value = NA, name='Forage Density')
  }

  if (isFALSE(axes_units)){
    p <- p + ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank())
  }

  if (isTRUE(scalebar)) {
    p <- p + ggspatial::annotation_scale(location= scalebar_loc)
  }

  if (isTRUE(north_arrow)) {
    p <- p +
      ggspatial::annotation_north_arrow(location = north_arrow_loc, which_north = "true",
                                               height = ggplot2::unit(north_arrow_size, "cm"),
                                               width = ggplot2::unit(north_arrow_size, "cm"),
                                               style = ggspatial::north_arrow_orienteering(text_col=NA,
                                                                                           fill = c("black", "black")))
  }

  if (isTRUE(set_lims)){
    p <- p + ggplot2::scale_x_continuous(limits= c(plot_extent[1], plot_extent[2])) +
      ggplot2::scale_y_continuous(limits =c(plot_extent[3], plot_extent[4]))

  }


  if (isFALSE(wsg)) {
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(kd_raster), datum =  sf::st_crs(kd_raster))
  }

  if (isFALSE(guide)) {
    p <- p + ggplot2::guides(fill=FALSE)
  }

  return(p)
}



