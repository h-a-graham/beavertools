#' Built in plotting function for Kernel density Raster.
#'
#'This function provides a simple way to produce consistent maps of Kernel density plots.
#'Please be aware that the 'basemap', 'rivers' and 'add_hillshade' arguments  use the following functions:
#'`rosm::osm.image()` `osmdata::opq()` which occasional fail during busy server times.
#'
#' @param kd_raster Kernel Density raster generated from the `beavertools::forage_density()`
#' @param basemap Boolean, include an OSM basemap. (optional)
#' @param basemap_type Character vector for osm map type. for options see `rosm::osm.types()`
#' @param trans_fill Boolean to transform the colourmap - visualisation general better when TRUE (the default)
#' @param trans_type Character vector for the type of transform.
#' @param axes_units Boolean to include coordinate values on axis.
#' @param scalebar Boolean to include a scalebar.
#' @param scalebar_loc character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to include a north arrow
#' @param north_arrow_loc character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric vector for the arrow
#' @param wgs Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the {osmdata} package) OR a river network of class
#' 'sf' which can be generated beforehand using `beavertools::get_rivers()`.
#' @param add_hillshade Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
#' create a textured basemap.
#' @param plot_extent 'bbox', 'sf' or 'sp' object used to set the plot extent.
#' @param attribute Boolean to include an open street map attribution.
#' @return ggplot object of Kernel Density Map
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#'
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#' # Now we plot the raster with plot_forage_density
#' beaver_forage <- plot_forage_density(ROBT_201920, catchment = Otter_catch, rivers = TRUE,
#'                                      plot_extent = target_ext, trans_fill=TRUE)
#'
plot_forage_density <- function(kd_raster, basemap=TRUE, basemap_type = "osmgrayscale", trans_fill = TRUE, trans_type = 'log10',
                     axes_units = TRUE, scalebar=TRUE, scalebar_loc = 'tl', north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                     wgs=TRUE, guide=TRUE, catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL, attribute = TRUE, guide_width =NULL){

  orig_crs <- sf::st_crs(kd_raster)
  kd_raster <- raster::projectRaster(kd_raster, crs=sp::CRS(sprintf('+init=EPSG:%s',sf::st_crs(4326)$epsg)))

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
    catch_mask <- create_mask(catchment)%>%
      sf::st_transform(crs = 4326)
    p <- p + ggspatial::annotation_spatial(catch_mask, fill = "grey50", alpha=0.5)
  }

  if (isTRUE(rivers)){
    if (is.null(catchment)) {
      message('"rivers" argument ignored. A valid "catchment" area is required.')
    } else {
      river_sf <- get_rivers(catchment)
      p <- p + ggspatial::annotation_spatial(river_sf, colour = "#5699FA", alpha=0.9, size=0.2)
    }
  } else if(class(rivers)[1] == "sf"){
    rivers <- rivers %>%
      sf::st_transform(crs = 4326)
    p <- p + ggspatial::annotation_spatial(rivers, colour = "#5699FA", alpha=0.9, size=0.2)
  }

  p <- p + ggspatial::layer_spatial(kd_raster, ggspatial::aes(fill=stat(band1)),alpha = 0.7) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_text(size=10))


  if (isTRUE(trans_fill)){
    p <- p + ggplot2::scale_fill_viridis_c(na.value = NA, name= sprintf('%s Forage Density', trans_type), trans=trans_type,
                                           n.breaks=2)
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

  if (isTRUE(set_lims) && isTRUE(wgs)){
    # p <- p + ggplot2::scale_x_continuous(limits= c(plot_extent[1], plot_extent[2])) +
    #   ggplot2::scale_y_continuous(limits =c(plot_extent[3], plot_extent[4]))

    p <- p + coord_sf(xlim=c(plot_extent[1], plot_extent[2]), ylim=c(plot_extent[3], plot_extent[4]),
                      crs=sf::st_crs(kd_raster))
  }

  if (isFALSE(wgs)) {
    if (isTRUE(set_lims)) {

      pe <- sf::st_bbox(plot_extent)
      pe[[1]] <- plot_extent[1]
      pe[[2]] <- plot_extent[3]
      pe[[3]] <- plot_extent[2]
      pe[[4]] <- plot_extent[4]

      pe <- sf::st_as_sfc(pe) %>%
        sf::st_set_crs(sf::st_crs(kd_raster))%>%
        sf::st_transform(orig_crs) %>%
        st_bbox() %>%
        define_extent_bbox()

      p <- p + ggplot2::coord_sf(xlim=c(pe[1], pe[2]), ylim=c(pe[3], pe[4]),
                                 crs = orig_crs, datum = orig_crs)
    } else{
      p <- p + ggplot2::coord_sf(crs = orig_crs, datum = orig_crs)
    }

  }

  if (isFALSE(guide)) {
    p <- p + ggplot2::guides(fill=FALSE)
  }

  if (!is.null(guide_width)){
    p <- p + ggplot2::guides(fill = guide_colourbar(barheight = guide_width))
  }

  if (isTRUE(basemap)|isTRUE(rivers)|isTRUE(add_hillshade)){
    if (isTRUE(attribute)){
      p <- p +
        ggplot2::labs(caption = '© OpenStreetMap contributors') +
        ggplot2::theme(plot.caption = ggplot2::element_text(size=6))
    }
  }


  return(p)
}



