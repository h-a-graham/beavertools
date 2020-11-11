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
