% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_capacity.R
\name{plot_capacity}
\alias{plot_capacity}
\title{Built in plotting function for territory carrying capacity.}
\usage{
plot_capacity(
  terr_capacity,
  buffer = 50,
  river_net = NULL,
  basemap = TRUE,
  basemap_type = "osmgrayscale",
  axes_units = TRUE,
  scalebar = TRUE,
  scalebar_loc = "tl",
  north_arrow = TRUE,
  north_arrow_loc = "br",
  north_arrow_size = 0.75,
  wgs = FALSE,
  guide = FALSE,
  catchment = NULL,
  rivers = FALSE,
  add_hillshade = FALSE,
  plot_extent = NULL,
  terr_colours = NULL,
  mask_fill = "grey50"
)
}
\arguments{
\item{terr_capacity}{output from \code{beavertools::territory_cap()} which gives the maximum number of territories that can
fit within the catchment.}

\item{buffer}{the buffer size to use to visualise the territories - larger buffers are better for visualising the territories
but can give the impression of overlap between territories.}

\item{river_net}{Supply the river network or BeaverNetwork useed to generate the terr_capacity object. This is added as a basemap layer
to display the river network.}

\item{basemap}{Boolean, include an OSM basemap. (optional)}

\item{basemap_type}{Character vector for osm map type. for options see \code{rosm::osm.types()}}

\item{axes_units}{Boolean to include coordinate values on axis.}

\item{scalebar}{Boolean to include a scalebar.}

\item{scalebar_loc}{character vector for the scalebar location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.}

\item{north_arrow}{Boolean to include a north arrow}

\item{north_arrow_loc}{character vector for the arrow location one of:'tl', 'bl', 'tr', 'br' Meaning "top left" etc.}

\item{north_arrow_size}{numeric vector for the arrow}

\item{wgs}{Boolean to transform coordinate reference system (CRS) to WGS84 (EPSG:4326)}

\item{guide}{Boolean to include a legend}

\item{catchment}{An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
This feature should be a boundary such as a catchment or Area of interest. It is used to mask the
map region outside of desired AOI.}

\item{rivers}{Boolean to include river lines (downloaded automatcally using the {osmdata} package).
Probably not reuired if 'river_net' argument is supplied.}

\item{add_hillshade}{Boolean to add an osm hillshade background map. This can be combined with 'basemap_type' to
create a textured basemap.}

\item{plot_extent}{'bbox', 'sf' or 'sp' object defining the desired plot extent.}

\item{terr_colours}{option to supply a custom colour palette. If NULL then a random colour palette is generated.}
}
\value{
A ggplot object which displays a map of the territory capacity.
}
\description{
Once territory capacity is generated using \code{beavertools::territory_cap()}, this function can be used to plot the results
in a standardised way.
}
\examples{

\dontrun{
# --- Subset dataset for example to reduce computation time ---
BeavNetOtter <- RivOtter_BeaverNet[RivOtter_BeaverNet$Str_order > 3,]

# ---------- run terriroty generation --------
test_out <-  gen_territories(BeavNetOtter)

# ------------- Run territory cap -------------
test_TC_par <-territory_cap(test_out, multicore = TRUE)

# Now plot...
plot_capacity(test_TC_par, basemap=FALSE)
}
}
