get_highest_ <- function(reach, network, progbar) {
  if (!is.null(progbar)) {
    progbar$tick()
  }

  network %>%
    dplyr::filter(lengths(sf::st_intersects(., reach)) > 0) %>%
    dplyr::arrange(., desc(mean_Str_order), desc(mean_BFI_40m)) %>%
    dplyr::slice(1L)
}

terr_sort_ <- function(Terrs, progbar) {
  Terrs %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::group_by(id) %>%
    dplyr::group_split() %>%
    purrr::map(., ~ get_highest_(., Terrs, progbar)) %>%
    dplyr::bind_rows() %>%
    unique.data.frame()
}

select_terr_ <- function(all_terrs, n_terr = NULL, pass = 1, progbar = NULL) {
  if (is.null(n_terr)) {
    n_terr = nrow(all_terrs)
  }

  if (isTRUE(progbar)) {
    message(sprintf('Internal Iterator: %s', pass))
    pb <- progress::progress_bar$new(total = nrow(all_terrs), clear = FALSE)
  } else {
    pb <- NULL
  }

  terr_pass <- all_terrs %>%
    terr_sort_(., progbar = pb)

  if (n_terr == nrow(terr_pass)) {
    return(terr_pass)
  } else {
    select_terr_(
      terr_pass,
      n_terr = nrow(terr_pass),
      pass = pass + 1,
      progbar = progbar
    )
  }
}


best_territories_ <- function(
  territories,
  terr_original = NULL,
  keep_terr = NULL,
  pass = 1,
  progbar = NULL
) {
  if (is.null(terr_original)) {
    terr_original <- territories
  }

  if (isTRUE(progbar)) {
    message(sprintf('External Iterator: %s', pass))
  }
  terr_step <- select_terr_(territories, progbar = progbar)

  if (is.null(keep_terr)) {
    keep_terr <- terr_step
  } else {
    keep_terr <- keep_terr %>%
      dplyr::bind_rows(terr_step)
  }

  avail_terr <- terr_original %>%
    dplyr::filter(!lengths(sf::st_intersects(., keep_terr)) > 0)

  if (nrow(avail_terr) == 0) {
    return(keep_terr)
  } else {
    best_territories_(
      avail_terr,
      terr_original,
      keep_terr,
      pass = pass + 1,
      progbar = progbar
    )
  }
}

#' Beaver territory capacity simulation
#'
#' Function to estimate the territory capacity of a catchment for beavers. The potential territories are iteratively compared
#' to select the most suitable habitats that do not intersect. Stream size and habitat quality are prioritised such that terrtories
#' in larger rivers with better habitat are considered more suitable.
#'
#' @param territories The potential terrtory areas produced using `beavertools::gen_territories()`
#' @param min_veg  Numeric vector describing the minimum vegetation index value which can support a beaver territory.
#' @param min_bdc  Numeric vector describing the minimum Beaver Dam Capacity for reaches where Stream Order <=4.
#' @param progbars Boolean to use a progress bar to monitor progress
#' @param multicore  Boolean to multiple core - This function can be slow for large catchments so setting as TRUE can speed things up.
#' @param ncores numeric denoting the number of processes to run the function across. If not included, defaults to:
#' `parallel::detectCores()[1]-2`
#' @return An 'sf' object containing all viable territories, the total of which equals the territory capacity.
#' @import foreach doParallel parallel tcltk
#' @export
#' @examples
#' \dontrun{
#' # --- Subset dataset for example to reduce computation time ---
#' BeavNetOtter <- RivOtter_BeaverNet[RivOtter_BeaverNet$Str_order > 3,]
#'
#' # ---------- run terriroty generation --------
#' poss_terrs <-  gen_territories(BeavNetOtter)
#'
#' # ------------- Run territory cap -------------
#' territory_cap(poss_terrs, multicore = TRUE)
#' }
#'
territory_cap <- function(
  territories,
  min_veg = 2.5,
  min_bdc = 1,
  progbars = TRUE,
  multicore = FALSE,
  ncores
) {
  terrs <- territories %>%
    dplyr::filter(mean_BFI_40m >= min_veg) %>%
    dplyr::mutate(
      discard = ifelse(Str_Ord < 5 & mean_BDC <= min_bdc, 'True', 'False')
    ) %>%
    dplyr::filter(discard == 'False') %>%
    dplyr::select(!discard)

  rem_terr_n <- nrow(territories) - nrow(terrs)

  if (rem_terr_n > 0) {
    warning(sprintf(
      'min_veg and min_bdc args have resulted in the removal of %s territories',
      rem_terr_n
    ))
  }

  if (isFALSE(multicore)) {
    # run program on single core.
    final_terrs <- best_territories_(terrs, progbar = progbars)
  } else {
    if (missing(ncores)) {
      cores = parallel::detectCores()[1] - 2
    } else {
      cores <- ncores
    }

    split_df <- terrs %>%
      dplyr::group_by((dplyr::row_number() - 1) %/% (dplyr::n() / cores)) %>%
      dplyr::group_split()

    inter_func <- function(split_t, orig_t) {
      orig_t %>%
        dplyr::filter(lengths(sf::st_intersects(., split_t)) > 0)
    }

    cl <- parallel::makeCluster(cores)

    doParallel::registerDoParallel(cl)

    inter_split <- split_df %>%
      purrr::map(., ~ inter_func(., terrs))

    par_terrs <- foreach::foreach(
      i = seq_along(split_df),
      .combine = rbind,
      .packages = c("dplyr", "sf", "purrr", "magrittr", "progress"),
      .export = c(
        "best_territories_",
        "terr_sort_",
        "terr_checks",
        "get_highest_"
      )
    ) %dopar%
      {
        return_terrs <- best_territories_(split_df[[i]], inter_split[[i]])

        return(return_terrs)
      }
    parallel::stopCluster(cl)

    clean_terrs <- unique.data.frame(par_terrs)

    final_int_t <- inter_func(clean_terrs, terrs)

    final_terrs <- best_territories_(clean_terrs, final_int_t)
  }

  accept_names <- c(
    "id",
    "Terr_Leng",
    "mean_BFI_40m",
    "mean_BDC",
    "mean_Str_order",
    "Str_Ord",
    "geom",
    "geometry"
  )
  junk_names <- colnames(final_terrs)[!accept_names %in% colnames(final_terrs)]

  final_terrs <- final_terrs %>%
    dplyr::select(!junk_names) %>%
    dplyr::arrange(., desc(mean_Str_order), desc(mean_BFI_40m)) %>%
    dplyr::mutate(id = dplyr::row_number())

  return(final_terrs)
}
