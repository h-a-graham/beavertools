#' Animate the spread of simulated territories from `territory_cap`
#'
#' @export
animate_capacity <- function(terr_capacity, buffer = 50, river_net=NULL, basemap=TRUE, basemap_type = "osmgrayscale",  axes_units = TRUE,
                             scalebar=TRUE, scalebar_loc = 'tl', north_arrow = TRUE, north_arrow_loc = 'br', north_arrow_size = 0.75,
                             wsg=FALSE, guide=FALSE, catchment=NULL, rivers=FALSE, add_hillshade = FALSE, plot_extent=NULL,
                             filename=NULL, x_pix=1200, y_pix=675, pix_res=96, n_frames=3, fps = 5){

  single_terr <- function(.n, .colour, .terr){

    .terr <- .terr[1:.n,]
    .colour <- .colour[1:.n]

    plot_capacity(.terr, buffer = buffer,  river_net = river_net,
                  basemap=basemap, basemap_type = basemap_type,  axes_units = axes_units,
                  scalebar=scalebar, scalebar_loc = scalebar_loc, north_arrow = north_arrow, north_arrow_loc = north_arrow_loc,
                  north_arrow_size = north_arrow_size,
                  wsg=wsg, guide=guide, catchment=catchment, rivers=rivers, add_hillshade = add_hillshade, plot_extent=plot_extent,
                  terr_colours = .colour)
  }

  ran_cols <- random_palette(nrow(terr_capacity))
  terr_plot_list <- seq_along(1:nrow(terr_capacity)) %>%
    purrr::map(., ~single_terr(., ran_cols, terr_capacity)) %>%
    animate_maps(., filename = filename, x_pix=x_pix, y_pix=y_pix,
                 pix_res=pix_res, n_frames=3, fps = 5)

}

