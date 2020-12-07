# ' Built in plotting function for territory carrying capacity.
#' @export
plot_capacity <- function(terr_capacity, buffer = 50, river_net=NULL, basemap=TRUE, basemap_type = "osmgrayscale",  axes_units = TRUE,
                          scalebar=TRUE, scalebar_loc = 'tl', north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                          wsg=FALSE, guide=FALSE, catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL, terr_colours = NULL){

  if (buffer > 0){
    terr_capacity <- terr_capacity %>%
      sf::st_buffer(., buffer, endCapStyle='FLAT')
  }

  if (!is.null(river_net)){
    river_net <- check_spatial_feature(river_net, 'catchment')
  }

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


  if (!is.null(river_net)) {
    p <- p + ggspatial::layer_spatial(river_net, colour = "#5699FA", alpha=0.9, size=0.2)
  } else if (isTRUE(rivers)){
    if (is.null(catchment)) {
      message('"rivers" argument ignored. A valid "catchment" area is required.')
    } else {
      river_sf <- get_rivers(catchment)
      p <- p + ggspatial::annotation_spatial(river_sf, colour = "#5699FA", alpha=0.9, size=0.2)
    }

  }

  p <- p + ggspatial::layer_spatial(terr_capacity, ggspatial::aes(fill=as.factor(id)), colour = 'grey50', lwd=0.005, alpha=0.8) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_text(size=10))

  if (is.null(terr_colours)){
    p <- p + ggplot2::scale_fill_manual(values = random_palette(nrow(terr_capacity)))
  } else {
    p <- p + ggplot2::scale_fill_manual(values = terr_colours)
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
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(terr_capacity), datum =  sf::st_crs(terr_capacity))
  }

  if (isFALSE(guide)) {
    p <- p + ggplot2::guides(fill=FALSE)
  }

  return(p)
}



