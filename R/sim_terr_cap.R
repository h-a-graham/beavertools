#' A function to run a territory capacity simulation
#'
#' This method enables for the calculation of uncertainty based on n random territory simulations and a specified
#' range of minimum habitat requirement.
#'
#' @export
sim_terr_cap <- function(BeaverNetwork, n_p_terr_sim=1, n_hab_sim=5, min_veg = c(2.5, 4), progbar=TRUE){

  n_terr_sims <- c(1:n_p_terr_sim)

  hab_mins <- seq(from=min_veg[1], to = min_veg[2], length.out=n_hab_sim)



  hab_sim <- function(.val, .p_terrs, pb = NULL) {

    if (!is.null(pb)){
      pb$tick()
    }

    test_TC_par <- territory_cap(.p_terrs, min_veg = .val , progbars=FALSE, multicore = TRUE) %>%
      dplyr::summarise(., mean_length = mean(Terr_Leng), n = dplyr::n()) %>%
      dplyr::select(mean_length, n) %>%
      dplyr::mutate(min_BFI = .val)
  }

  terr_sim <- function(.simNum, .BeavNet, .habVals, pb, target){

    if (isTRUE(pb)){
      message(sprintf('Simulation %s/%s:', .simNum, target))
      prog <- progress::progress_bar$new(total = length(hab_mins), clear = FALSE)
    } else {
      prog <- NULL
    }

    p_terrs <-  gen_territories(.BeavNet, progbar = FALSE)

    sim_sum <- .habVals %>%
      purrr::map(., ~ hab_sim(., p_terrs, pb= prog)) %>%
      dplyr::bind_rows()

  }

  sim_df <- n_terr_sims %>%
    purrr::map(., ~ terr_sim(.,BeaverNetwork, hab_mins, pb = progbar, target = length(n_terr_sims))) %>%
    dplyr::bind_rows()

}

