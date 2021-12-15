#' Add user specified classification of territory areas
#'
#' Opportunity to add a new column 'user_class' which enables the user to confirm or reject
#' the automated assessment of territory areas.
#'
#' @param territory_poly a territory polygon generated with `beavertools::estimate_territories()`
#' @param territory  numeric vector containing the ID numbers for areas to be reclassified as 'Territory'. e.g. c(10, 28)
#' @param possible numeric vector containing the ID numbers for areas to be reclassified as 'Possible'
#' @param activity numeric vector containing the ID numbers for areas to be reclassified as 'Activity'
#' @return territory_poly is returned with the an additional column 'user_class'
#' @export
#' @examples
#' # Here we filter the filter the built in 2019-2020 ROBT feeding sign data `RivOtter_FeedSigns`
#' # Then pipe this 'sf' object to forage_density.
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
#' user_classify(otter_poly, territory = c(10, 28))
user_classify <- function(territory_poly, territory=NULL, possible=NULL, activity=NULL){

  class_overlap <- territory %in% possible

  if (isTRUE(TRUE %in% class_overlap)){
    stop('A territory is duplicated in both possible and territory lists')
  }

  territory_poly %>%
    # dplyr::mutate(user_class = terr_status) %>%
    dplyr::mutate(user_class = as.factor(ifelse(id %in% territory, 'Territory',
                                      ifelse(id %in% possible, 'Possible',
                                             ifelse(id %in% activity, 'Activity',
                                                    as.character(terr_status)))))) %>%
    dplyr::relocate(user_class, .after = terr_status)


}
