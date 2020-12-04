# ' Built in plotting function for territory polygons.
#' @export
plot_territories <- function(terr_poly, fill_name, fill_col = c("#7EAAC7", "#F87223", "#61E265"),
                             label = FALSE, basemap=TRUE, basemap_type = "osmgrayscale", axes_units = TRUE,
                             scalebar=TRUE, scalebar_loc = 'tl',
                             north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                             wsg=FALSE, guide=TRUE, seed=NA, drop_act=FALSE, trans_type=NULL,
                             catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL){

  # define extent
  set_lims <- TRUE

  if (is.null(plot_extent)){
    set_lims <- FALSE
  } else {
    plot_extent <- create_plot_ext(plot_extent)
  }



  if (isTRUE(drop_act)){
    if ('user_class' %in% colnames(terr_poly)){
      terr_poly <- dplyr::filter(terr_poly, user_class != 'Activity')
    } else {
      terr_poly <- dplyr::filter(terr_poly, terr_status != 'Activity')
    }
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

  p <- p + ggplot2::geom_sf(terr_poly, mapping = ggplot2::aes(fill=!! dplyr::sym(fill_name)),alpha = 0.7, lwd=0.3) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_text(size=10))


  if (fill_name %in% c('terr_status', 'user_class')) {
    if(fill_name=='terr_status'){
      leg_tit <- 'Auto territory status'
    } else {
      leg_tit <- 'User territory status'
    }
    p <- p + ggplot2::scale_fill_manual(values = c(fill_col[1],fill_col[2], fill_col[3]),
                                        limits = levels(c('Activity', 'Possible', 'Territory')),
                                        name=leg_tit)
  } else if(fill_name == 'mean_fd') {
    if (is.null(trans_type)){
      p <- p + ggplot2::scale_fill_viridis_c(name= 'Mean Forage Density')
    } else {
      p <- p + ggplot2::scale_fill_viridis_c(name= 'Mean Forage Density', trans=trans_type)
    }

  } else if(fill_name == 'sum_fd') {
    if (is.null(trans_type)){
      p <- p + ggplot2::scale_fill_viridis_c(name= 'Sum Forage Density')
    } else {
      p <- p + ggplot2::scale_fill_viridis_c(name= 'Sum Forage Density', trans=trans_type)
    }
  } else if(fill_name == 'id') {

    # Define the number of colors you want
    nb.cols <- nrow(terr_poly)
    # mycolors <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(nb.cols)
    if (!is.na(seed)){
      set.seed(seed)
    }
    mycolors <-random_palette(nb.cols)

    p <- p + ggplot2::scale_fill_manual(values = mycolors)
  }

  if (isTRUE(label)){
    p <- p + ggrepel::geom_label_repel(dplyr::filter(terr_poly, terr_status!='Activity'),
                             mapping = aes(label = id, geometry=geometry), position =position_dodge2(width=10),
                             arrow = arrow(length = unit(0.02, "npc"), angle = 25, type = "open", ends = "first"),
                             force = 10, inherit.aes = F, fill=NA, seed=seed, show.legend=F,stat = "sf_coordinates")
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
    p <- p + ggplot2::coord_sf(crs = sf::st_crs(terr_poly), datum =  sf::st_crs(terr_poly))
  }

  if (isFALSE(guide)) {
    p <- p + ggplot2::guides(fill=FALSE)
  }

  rm(.Random.seed, envir=globalenv())

  return(p)
}


# ' Built in plotting function to check automated territory class assignment.
#' @export
check_auto_terr <- function(terr_poly, fill_col = c("#7EAAC7", "#F87223", "#61E265"), label = TRUE,
                            basemap=FALSE, basemap_type = "osmgrayscale", axes_units = TRUE,
                            scalebar=TRUE, scalebar_loc = 'tl',
                            north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                            wsg=FALSE, guide=TRUE, plot_extent){
if (missing(plot_extent)){
  plot_territories(terr_poly=terr_poly, fill_name='terr_status', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wsg=wsg, guide=guide)
} else {
  plot_territories(terr_poly=terr_poly, fill_name='terr_status', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wsg=wsg, guide=guide, plot_extent=plot_extent)
}



}


# ' Built in plotting function to check automated territory class assignment.
#' @export
check_user_terr <- function(terr_poly, fill_col = c("#7EAAC7", "#F87223", "#61E265"), label = TRUE,
                            basemap=FALSE, basemap_type = "osmgrayscale", axes_units = TRUE,
                            scalebar=TRUE, scalebar_loc = 'tl',
                            north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                            wsg=FALSE, guide=TRUE, plot_extent){
  if (missing(plot_extent)){
  plot_territories(terr_poly=terr_poly, fill_name = 'user_class', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wsg=wsg, guide=guide)
  } else {
    plot_territories(terr_poly=terr_poly, fill_name = 'user_class', fill_col=fill_col, label=label,
                     basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                     scalebar=scalebar, scalebar_loc=scalebar_loc,
                     north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                     wsg=wsg, guide=guide, plot_extent=plot_extent)
}


}







