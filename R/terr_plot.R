#' Built in plotting function for territory polygons.
#'
#' function to create a ggplot for the territory zones.
#'
#' @param terr_poly a territory polygon created using `beavertools::estimate_territories()`
#' @param fill_name a character vector containing the name of the column to be used as the fill aesthetic.
#' choose from: 'terr_status', 'user_class', 'mean_fd', sum_fd' or 'id'.
#' @param fill_col character vector of R colours or HEX codes.
#' @param label label activity areas with polygon ID.
#' @param basemap Boolean, include an OSM basemap. (optional)
#' @param basemap_type Character vector for osm map type. for options see `rosm::osm.types()`
#' @param axes_units Boolean to include coordinate values on axis.
#' @param scalebar Boolean to include a scalebar.
#' @param scalebar_loc character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to include a north arrow
#' @param north_arrow_loc character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric vector for the arrow
#' @param wgs Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param guide_pos character denoting the position of the legend c('left, 'right, 'bottom', 'top)
#' @param seed numeric seed number -useful if using 'fill_name' = 'ID' as will set the same random colour palette.
#' @param drop_act  Boolean to remove areas classified as 'activity' this creates a plot which shows only possibl
#' and confirmed territories
#' @param trans_type character - the transformation type to be used if fill_name=c('mean_fd', sum_fd').
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the {osmdata} package) OR a river network of class
#' 'sf' which can be generated beforehand using `beavertools::get_rivers()`.
#' @param add_hillshade Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
#' create a textured basemap.
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @return ggplot object of the territory check map.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#'
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#'# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
#'# year as the forage density raster.
#'
#' CS_201920 <- RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")
#'
#'# run territory classification
#' otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)
#'
#'# various options:
#'plot_territories(otter_poly_uc, 'user_class', basemap=TRUE)
#'plot_territories(otter_poly_uc, 'mean_fd', basemap=FALSE)
#'plot_territories(otter_poly_uc, 'sum_fd', basemap=FALSE, trans_type = 'log10')
#'plot_territories(otter_poly_uc, 'id', basemap=TRUE, guide = FALSE, label = TRUE,
#'                 drop_act = TRUE, axes_units = FALSE, rivers = TRUE)
#'
plot_territories <- function(terr_poly, fill_name, fill_col = c("#7EAAC7", "#F87223", "#61E265"),
                             label = FALSE, basemap=TRUE, basemap_type = "osmgrayscale", axes_units = TRUE,
                             scalebar=TRUE, scalebar_loc = 'tl',
                             north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                             wgs=TRUE, guide=TRUE, guide_pos = "right", seed=NA, drop_act=FALSE, trans_type=NULL,
                             catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL){

  orig_crs <- sf::st_crs(terr_poly)

  terr_poly <- terr_poly%>%
    sf::st_transform(crs = 4326)

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
    catch_mask <- create_mask(catchment) %>%
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

 # dealing with points first
  if (fill_name =='feedsigns'){
    weight_levs <- terr_poly %>%
      select(!! dplyr::sym(fill_name)) %>%
      sf::st_drop_geometry() %>%
      dplyr::arrange(dplyr::desc(!! dplyr::sym(fill_name)))%>%
      unique() %>%
      pull()

    if (length(weight_levs)==3){
      p <- p + ggplot2::geom_sf(terr_poly, mapping = ggplot2::aes(colour=FeedCat,
                                                                  size=!! dplyr::sym(fill_name)),alpha = 0.7,
                                inherit.aes = FALSE) +
        ggplot2::scale_colour_manual(values = c(fill_col[1],fill_col[2], fill_col[3]), breaks = (c('Low', 'Med', 'High')),
                                     labels = (c('Low', 'Med', 'High')), name='Impact level') +
        ggplot2::scale_size(range = c(weight_levs[3], weight_levs[1]),
                            breaks = (c(weight_levs[3], weight_levs[2], weight_levs[1])),
                            labels = (c('Low', 'Med', 'High')),
                            name='Impact level')
    } else if (length(weight_levs)==1){
      p <- p + ggplot2::geom_sf(terr_poly, mapping = ggplot2::aes(colour=FeedCat),alpha = 0.7, size = weight_levs) +
        ggplot2::scale_colour_manual(values = c(fill_col[1],fill_col[2], fill_col[3]), breaks = (c('Low', 'Med', 'High')),
                                     labels = (c('Low', 'Med', 'High')), name='Impact level')

    }



  } else if (fill_name == 'othersigns'){
    p <- p + ggplot2::geom_sf(terr_poly, mapping = ggplot2::aes(colour=othersigns),alpha = 0.5,
                              size = dplyr::pull(terr_poly, p_size)[1]) +
      ggplot2::scale_colour_manual(values = c(fill_col[1],fill_col[2]), name='Sign type',
                                   labels = c('Dam', 'Dwelling'), breaks = c('Dam', 'Dwelling'),drop=FALSE)

  } else { # Now we tackle polygon requests

    p <- p + ggplot2::geom_sf(terr_poly, mapping = ggplot2::aes(fill=!! dplyr::sym(fill_name)),alpha = 0.7, lwd=0.3)


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
      if (!is.na(seed)){
        set.seed(seed)
      }
      mycolors <-random_palette(nb.cols)

      p <- p + ggplot2::scale_fill_manual(values = mycolors)
    }
  }

  #set theme stuff
  p <- p + ggplot2::theme_bw() +
    ggplot2::theme(legend.title=ggplot2::element_text(size=10))



  if (isTRUE(label)){
    p <- p + ggplot2::geom_sf_label(dplyr::filter(terr_poly, terr_status!='Activity'),
                             mapping = aes(label = id, geometry=geometry),
                             angle = 25, inherit.aes = F, fill='grey70', alpha=0.5,  show.legend=F)
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
                crs=sf::st_crs(terr_poly))
  }

  if (isFALSE(wgs)) {
    if (isTRUE(set_lims)) {

      pe <- sf::st_bbox(plot_extent)
      pe[[1]] <- plot_extent[1]
      pe[[2]] <- plot_extent[3]
      pe[[3]] <- plot_extent[2]
      pe[[4]] <- plot_extent[4]

      pe <- sf::st_as_sfc(pe) %>%
        sf::st_set_crs(sf::st_crs(terr_poly))%>%
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
    p <- p + ggplot2::guides(fill=FALSE, colour=FALSE, size=FALSE)
  } else {
    p <- p + theme(legend.position = guide_pos)
  }

  rm(.Random.seed, envir=globalenv())

  return(p)
}


#' Built in plotting function to check automated territory class assignment.
#'
#' Function plots the automatically generated territory classifications with corresponding ID numbers
#' take note of the numbers which have been missclassified and correct them using `beavertools::user_classify()`
#'
#' @param terr_poly a territory polygon created using `beavertools::estimate_territories()`
#' @param fill_col character vector of R colours or HEX codes.
#' @param label label activity areas with polygon ID. important when checking the predicted classification
#' @param basemap Boolean, include an OSM basemap. (optional)
#' @param basemap_type Character vector for osm map type. for options see `rosm::osm.types()`
#' @param axes_units Boolean to include coordinate values on axis.
#' @param scalebar Boolean to include a scalebar.
#' @param scalebar_loc character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to include a north arrow
#' @param north_arrow_loc character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric vector for the arrow
#' @param wgs Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @return ggplot object of the territory check map.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#'
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#'# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
#'# year as the forage density raster.
#'
#' CS_201920 <- RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")
#'
#'# run territory classification
#' otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)
#'
#'# create the map for checking automated territory classification
#' check_auto_terr(otter_poly, basemap=FALSE, label=TRUE)
#'
check_auto_terr <- function(terr_poly, fill_col = c("#7EAAC7", "#F87223", "#61E265"), label = TRUE,
                            basemap=FALSE, basemap_type = "osmgrayscale", axes_units = TRUE,
                            scalebar=TRUE, scalebar_loc = 'tl',
                            north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                            wgs=TRUE, guide=TRUE, plot_extent){
if (missing(plot_extent)){
  plot_territories(terr_poly=terr_poly, fill_name='terr_status', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wgs=wgs, guide=guide)
} else {
  plot_territories(terr_poly=terr_poly, fill_name='terr_status', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wgs=wgs, guide=guide, plot_extent=plot_extent)
}



}


#' Built in plotting function to check user-corrected territory class assignment.
#'
#' Function to plot the user-corrected territory classes from: `beavertools::user_classify()`
#'
#' @param terr_poly a territory polygon created using `beavertools::estimate_territories()`
#' @param fill_col character vector of R colours or HEX codes.
#' @param label label activity areas with polygon ID. important when checking the predicted classification
#' @param basemap Boolean, include an OSM basemap. (optional)
#' @param basemap_type Character vector for osm map type. for options see `rosm::osm.types()`
#' @param axes_units Boolean to include coordinate values on axis.
#' @param scalebar Boolean to include a scalebar.
#' @param scalebar_loc character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow Boolean to include a north arrow
#' @param north_arrow_loc character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.
#' @param north_arrow_size numeric vector for the arrow
#' @param wgs Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @return ggplot object of the territory check map.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
#'
#' ROBT_201920 <- RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")%>%
#'   forage_density(., 'FeedCat')
#'
#'# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
#'# year as the forage density raster.
#'
#' CS_201920 <- RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020")
#'
#'# run territory classification
#' otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)
#'
#'# create the map for checking automated territory classification
#' otter_poly_uc <- user_classify(otter_poly, territory = c(10, 28))
#'
#' # generate the user territory check plot.
#' check_user_terr(otter_poly_uc, basemap=FALSE)
#'
check_user_terr <- function(terr_poly, fill_col = c("#7EAAC7", "#F87223", "#61E265"), label = TRUE,
                            basemap=FALSE, basemap_type = "osmgrayscale", axes_units = TRUE,
                            scalebar=TRUE, scalebar_loc = 'tl',
                            north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                            wgs=TRUE, guide=TRUE, plot_extent){
  if (missing(plot_extent)){
  plot_territories(terr_poly=terr_poly, fill_name = 'user_class', fill_col=fill_col, label=label,
                   basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                   scalebar=scalebar, scalebar_loc=scalebar_loc,
                   north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                   wgs=wgs, guide=guide)
  } else {
    plot_territories(terr_poly=terr_poly, fill_name = 'user_class', fill_col=fill_col, label=label,
                     basemap=basemap, basemap_type=basemap_type, axes_units=axes_units,
                     scalebar=scalebar, scalebar_loc=scalebar_loc,
                     north_arrow=north_arrow, north_arrow_loc=north_arrow_loc, north_arrow_size=north_arrow_size,
                     wgs=wgs, guide=guide, plot_extent=plot_extent)
}


}







