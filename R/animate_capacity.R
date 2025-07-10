#' Animate the spread of simulated territories from `beavertools::territory_cap()`
#'
#'
#' This (somewhat experimental) funtion allows the user to simulate the expansion of beaver populations.
#' At present this isvery simple and territories in the largest channels with the best habitat are occupied first.
#' We intend to develop this function to allow for alternative expansion hypotheses.
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
#' @param wgs Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)
#' @param guide Boolean to include a legend
#' @param catchment An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
#' map region outside of desired AOI.
#' @param rivers Boolean to include river lines (downloaded automatcally using the osmdata package).
#' Probably not reuired if 'river_net' argument is supplied.
#' @param plot_extent 'bbox', 'sf' or 'sp' object defining the desired plot extent.
#' @param filename character string for the save path if saving to disk is required.
#' @param x_pix numeric denoting width of the animation canvas in pixels
#' @param y_pix numeric denoting height of the animation canvas in pixels
#' @param pix_res numeric denoting canvas resolution
#' @param n_frames numeric for the number of frames to use for transitions
#' @param fps numeric for the number of frames per second for the resulting animation
#' @return An animated map of territory expansion up to capacity.
#' @export
#' @examples
#'
#' \dontrun{
#' # ---------- Subset dataset for example to reduce computation time -----------
#'
#' BeavNetOtter <- RivOtter_BeaverNet[RivOtter_BeaverNet$Str_order > 3,]
#'
#' # ---------- run terriroty generation --------
#' test_out <-  gen_territories(BeavNetOtter)
#'
#' # ------------- Run territory cap -------------
#' test_TC_par <-territory_cap(test_out, multicore = TRUE)
#'
#' # now animate the spead of territories through catchment
#' animate_capacity(test_TC_par, buffer = 75, river_net = BeavNetOtter,
#'                            x_pix=600, y_pix=675)
#' }
animate_capacity <- function(
  terr_capacity,
  buffer = 50,
  river_net = NULL,
  basemap = TRUE,
  basemap_type = "cartolight",
  axes_units = TRUE,
  scalebar = TRUE,
  scalebar_loc = 'tl',
  north_arrow = TRUE,
  north_arrow_loc = 'br',
  north_arrow_size = 0.75,
  wgs = FALSE,
  guide = FALSE,
  catchment = NULL,
  rivers = FALSE,
  plot_extent = NULL,
  filename = NULL,
  x_pix = 1200,
  y_pix = 675,
  pix_res = 96,
  n_frames = 3,
  fps = 5
) {
  match.arg(basemap_type, rosm::osm.types())
  single_terr <- function(.n, .colour, .terr) {
    .terr <- .terr[1:.n, ]
    .colour <- .colour[1:.n]

    plot_capacity(
      .terr,
      buffer = buffer,
      river_net = river_net,
      basemap = basemap,
      basemap_type = basemap_type,
      axes_units = axes_units,
      scalebar = scalebar,
      scalebar_loc = scalebar_loc,
      north_arrow = north_arrow,
      north_arrow_loc = north_arrow_loc,
      north_arrow_size = north_arrow_size,
      wgs = wgs,
      guide = guide,
      catchment = catchment,
      rivers = rivers,
      plot_extent = plot_extent,
      terr_colours = .colour
    )
  }

  ran_cols <- random_palette(nrow(terr_capacity))
  terr_plot_list <- seq_along(1:nrow(terr_capacity)) %>%
    purrr::map(., ~ single_terr(., ran_cols, terr_capacity)) %>%
    animate_maps(
      .,
      filename = filename,
      x_pix = x_pix,
      y_pix = y_pix,
      pix_res = pix_res,
      n_frames = 3,
      fps = 5
    )
}
