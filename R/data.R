#' Beaver Feeding Sign Locations recorded during the River Otter Beaver Trial.
#'
#' A dataset (sf object) which includes the locations, survey times,
#' survey season and impact level of each sign.
#'
#' @format An sf object with 3683 rows and 4 variables:
#' \describe{
#'   \item{SurveySeason}{The winter season during which the sign was surveyed}
#'   \item{RecordDate}{The date that a sign was recorded}
#'   \item{FeedCat}{Character String describing the effort exerted by beaver on a given sign}
#'   \item{geometry}{The geometry data for the point location}
#' }
#' @source \url{https://www.exeter.ac.uk/creww/research/beavertrial/}
"RivOtter_FeedSigns"



#' Beaver Dwelling and Dam Locations recorded during the River Otter Beaver Trial.
#'
#' A dataset ('sf' object) which includes the locations, survey times and
#' survey season for beaver dwellings and dams. These data can be used to confirm or
#' reject the automated territory status. Essentially, it is considered that where dams
#' or dwellings occur, an area of activity can be considered as a territory.
#'
#' @format An sf object with 3683 rows and 4 variables:
#' \describe{
#'   \item{SurveySeason}{The winter season during which the sign was surveyed}
#'   \item{RecordDate}{The date that a sign was recorded}
#'   \item{SignType}{Character String describing the type of sign}
#'   \item{geometry}{The geometry data for the point location}
#' }
#' @source \url{https://www.exeter.ac.uk/creww/research/beavertrial/}
"RivOtter_OtherSigns"

#' River Otter Catchment Area.
#'
#' A dataset ('sf' object) provides the River Otter Catchment area. This can be used in various plotting
#' functions to mask areas outside of the Otter catchment.
#'
#' @format An sf object with 1 rows and 3 variables:
#' \describe{
#'   \item{Name}{Simply the name of the catchment}
#'   \item{geometry}{The geometry data for the catchment}
#' }
#' @source \url{generated from OS Terrain 5: https://www.ordnancesurvey.co.uk/business-government/products/terrain-5 using GRASS GIS (https://grass.osgeo.org/)}
"RivOtter_Catch_Area"


#' River Otter Beaver Network.
#'
#' A dataset ('sf' object) provides the River Otter Beaver Network. This is a line feature netowkr for the rivers in the R. Otter catchment
#' with associated reach values for Beaver Dam Capacity (BDC) and Beaver Forage Index (BFI). This dataset can be used in the territory simulation
#' functions to estimate the number of territories that can be supported within the network. River Network is from the OS Open Rivers Network.
#' Methods for generating the Beaver Network can be found in this paper: https://doi.org/10.1007/s10344-020-01379-w and the
#' current repo can be found here: https://github.com/h-a-graham/BDC_V1.2. A National scale Beaver Network is to be released asap.
#'
#' @format An sf object with 1289 features and 26 fields:
#' \describe{
#'   \item{BDC}{Beaver Dam Capacity}
#'   \item{BDC_cat}{The category assigned based on BDC.}
#' }
#' @source \url{https://doi.org/10.1007/s10344-020-01379-w}
"RivOtter_BeaverNet"
