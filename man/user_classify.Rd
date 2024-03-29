% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_classify.R
\name{user_classify}
\alias{user_classify}
\title{Add user specified classification of territory areas}
\usage{
user_classify(
  territory_poly,
  territory = NULL,
  possible = NULL,
  activity = NULL
)
}
\arguments{
\item{territory_poly}{a territory polygon generated with \code{beavertools::estimate_territories()}}

\item{territory}{numeric vector containing the ID numbers for areas to be reclassified as 'Territory'. e.g. c(10, 28)}

\item{possible}{numeric vector containing the ID numbers for areas to be reclassified as 'Possible'}

\item{activity}{numeric vector containing the ID numbers for areas to be reclassified as 'Activity'}
}
\value{
territory_poly is returned with the an additional column 'user_class'
}
\description{
Opportunity to add a new column 'user_class' which enables the user to confirm or reject
the automated assessment of territory areas.
}
\examples{
# Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
# Then pipe this 'sf' object to forage_density.
ROBT_201920 <- RivOtter_FeedSigns \%>\%
dplyr::filter(SurveySeason == "2019 - 2020")\%>\%
  forage_density(., 'FeedCat')

# Now we load the ROBT `RivOtter_OtherSigns` dataset and filter to the same
# year as the forage density raster.

CS_201920 <- RivOtter_OtherSigns \%>\%
dplyr::filter(SurveySeason == "2019 - 2020")

# run territory classification
otter_poly <- estimate_territories(ROBT_201920, confirm_signs = CS_201920)

# create the map for checking automated territory classification
check_auto_terr(otter_poly, basemap=FALSE, label=TRUE)

user_classify(otter_poly, territory = c(10, 28))
}
