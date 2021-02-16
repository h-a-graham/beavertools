#' Built in plotting function for territory carrying capacity.
#'
#'
#'
#' @param terr_capacity output from `beavertools::territory_cap()` which gives the maximum number of territories that can
#' fit within the catchment.
#' @param buffer the buffer size to use to visualise the territories - larger buffers are better for visualising the territories
#' but can give the impression of overlap between territories.
#' @param river_net Supply the river network or BeaverNetwork useed to generate the terr_capacity object. This is added as a basemap layer
#' to display the river network.
#' @param basemap Boolean, include an OSM basemap. (optional)
#' @param basemap_type Character vector for osm map type. for options see `rosm::osm.types()`
#' @param axes_units Boolean to include coordinate values on axis.
#' @param scalebar Boolean to include a scalebar.
#' @param scalebar_loc character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to include a north arrow
#' @param north_arrow_loc character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric vector for the arrow
#' @param wsg Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the {osmdata} package).
#' Probably not reuired if 'river_net' argument is supplied.
#' @param add_hillshade Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
#' create a textured basemap.
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @param terr_colours  option to supply a custom colour palette. If NULL then a random colour palette is generated.
#' @return A ggplot object which displays a map of the territory capacity.
#' @export
#' @examples
#' # here we read in the BeaverNetwork data
#' # NOTE - MUST ADD OPEN SOURCE VERSION AS BUILT IN DATA ASAP!
#' BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg')
#'
#' # ---------- Subset dataset for example to reduce computation time -----------
#' BeavNetOtter <- BeavNetOtter[BeavNetOtter$Str_order > 3,]
#'
#' # ---------- run terriroty generation --------
#' test_out <-  gen_territories(BeavNetOtter)
#'
#' # ------------- Run territory cap -------------
#' test_TC_par <-territory_cap(test_out, multicore = TRUE)
#'
#' # Now plot...
#' plot_capacity(test_TC_par)
#'
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



