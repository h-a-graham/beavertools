
#' Plotting function for feeding/foraging Locations.
#'
#' function to create a ggplot for the feeding points or confirmatory signs. This is just a lazy wrapper for
#' `beavertools::plot_territories` but tidies things up a bit. It is not exaustive and
#'
#' @param beav_points  An 'sf' object containing the location and impact level of beaver foraging signs
#' @param weight_aes a numeric vector of length 3 or 1. if length is one all points will appear the same size on the plot.
#' where the length equals 3, the value is used as the size aesthetic for points based on their impact category.
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
#' @param wsg Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param seed numeric seed number -useful if using 'fill_name' = 'ID' as will set the same random colour palette.
#' @param drop_act  Boolean to remove areas classified as 'activity' this creates a plot which shows only possibl
#' and confirmed territories
#' @param trans_type character - the transformation type to be used if fill_name=c('mean_fd', sum_fd').
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the {osmdata} package)
#' @param add_hillshade Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
#' create a textured basemap.
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @return ggplot object for a map of beaver feeding signs.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to plot_feeding.
#'
#' RivOtter_FeedSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020") %>%
#'   plot_feeding(., weight_aes=c(1,3,6), basemap=F,
#'                catchment = Otter_catch, rivers = T, plot_extent = target_ext)
#'
plot_feeding <- function(beav_points, weight_aes = c(1,3,6), fill_col = c("#1b9e77", "#7570b3", "#d95f02"),
                             label = FALSE, basemap=TRUE, basemap_type = "osmgrayscale", axes_units = TRUE,
                             scalebar=TRUE, scalebar_loc = 'tl',
                             north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                             wsg=FALSE, guide=TRUE, seed=NA, drop_act=FALSE, trans_type=NULL,
                             catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL){

  if (length(weight_aes)==1){
    weight_aes <- c(weight_aes,weight_aes,weight_aes)
  }

  beav_points %>%
        dplyr::mutate(feedsigns = ifelse(FeedCat == 'Low', weight_aes[1],
                      ifelse(FeedCat == 'Med', weight_aes[2], weight_aes[3]))) %>%
        dplyr::mutate(FeedCat = forcats::fct_relevel(FeedCat, 'High', 'Med', 'Low')) %>%
        plot_territories(., 'feedsigns', fill_col, label, basemap, basemap_type, axes_units,
                         scalebar, scalebar_loc, north_arrow, north_arrow_loc, north_arrow_size,
                         wsg, guide, seed, drop_act, trans_type, catchment, rivers, add_hillshade, plot_extent)

}

#' Plotting function for Confirmatory sign (Dwelling and Dam) locations.
#'
#' function to create a ggplot for the feeding points or confirmatory signs. This is just a lazy wrapper for
#' `beavertools::plot_territories` but tidies things up a bit. It is not exaustive and
#'
#' @param beav_points  An 'sf' object containing the location of beaver dwellings and dams.
#' Alternative signs will be labelled as 'other'
#' @param size a numeric vector of length 1 denoting the point size to be used by {ggplot2}.
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
#' @param wsg Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param seed numeric seed number -useful if using 'fill_name' = 'ID' as will set the same random colour palette.
#' @param drop_act  Boolean to remove areas classified as 'activity' this creates a plot which shows only possibl
#' and confirmed territories
#' @param trans_type character - the transformation type to be used if fill_name=c('mean_fd', sum_fd').
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the {osmdata} package)
#' @param add_hillshade Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
#' create a textured basemap.
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @return ggplot object of a confirmatory signs map.
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT confirmatory sign data `RivOtter_OtherSigns`
#' # Then pipe this 'sf' object to plot_feeding.
#'
#' RivOtter_OtherSigns %>%
#' dplyr::filter(SurveySeason == "2019 - 2020") %>%
#'   plot_other_signs(., size = 1.5,basemap=T, catchment = Otter_catch,
#'                    rivers = T, plot_extent = target_ext)
#'
plot_other_signs <- function(beav_points, size = 2.5, fill_col = c("#e41a1c", "#4daf4a", "#d11141"),
                         label = FALSE, basemap=TRUE, basemap_type = "osmgrayscale", axes_units = TRUE,
                         scalebar=TRUE, scalebar_loc = 'tl',
                         north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                         wsg=FALSE, guide=TRUE, seed=NA, drop_act=FALSE, trans_type=NULL,
                         catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL){

  beav_points %>%
    mutate(SignType = as.character(SignType)) %>%
    dplyr::mutate(othersigns = ifelse(SignType=='Dwelling',  SignType,
                                      ifelse(SignType=='Dam', SignType, 'Other'))) %>%
    dplyr::mutate(p_size = size) %>%
    plot_territories(., 'othersigns', fill_col, label, basemap, basemap_type, axes_units,
                     scalebar, scalebar_loc, north_arrow, north_arrow_loc, north_arrow_size,
                     wsg, guide, seed, drop_act, trans_type, catchment, rivers, add_hillshade, plot_extent)

}
