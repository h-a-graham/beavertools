#' A function to run a territory capacity simulation
#'
#' This function can be used to run territory capacity simulaitons, the randomised elements of the program include the
#' generation of random territory sizes for each reach. Secondly on each of these potential territory scenarios, a simulation
#' is carried out to test the effect of different minimum habitat requirements. This helps to provide some idea of the
#' uncertainty that is involved with the generated territory capacity estimates.
#'
#' @param BeaverNetwork A river network with attributed results from Graham, et al., (2020) or Macfarlane, et al., (2017)
#' An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' @param n_p_terr_sim Numeric denoting the number of random potential territory simulation to undertake.
#' @param n_hab_sim Numeric which defines the number of minimum habitat quality scenarios to test between the range given in 'min_veg'
#' @param min_veg Numeric vector of length two which sets the range of minimum habitat values to be tested in the simulation
#' @param progbar Boolean to use a progress bar to monitor progress
#' @return an 'sf' object containing all the results from the simulation. each row is a dissolved geometry of all simulated territories.
#' Additional information for each simulation includes the number of territories, the minimum vegetation value used and the simulation number.
#' @export
#' @examples
#' \dontrun{
#' # --- Subset dataset for example to reduce computation time  ---
#' BeavNetOtter <- RivOtter_BeaverNet[RivOtter_BeaverNet$Str_order > 3,]
#'
#' # --- run simple simulation...
#'  sim_terr_cap(BeavNetOtter, n_p_terr_sim=2, n_hab_sim=2, min_veg = c(1.3, 1.8))
#' }
sim_terr_cap <- function(BeaverNetwork, n_p_terr_sim=1, n_hab_sim=5, min_veg = c(1.5, 4), progbar=TRUE){

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

  terr_sim <- function(.simNum, .BeavNet, .habVals, pb, target, sim_n){

    if (isTRUE(pb)){
      message(sprintf('Simulation %s/%s:', .simNum, target))
      prog <- progress::progress_bar$new(total = length(hab_mins), clear = FALSE)
    } else {
      prog <- NULL
    }

    p_terrs <-  gen_territories(.BeavNet, progbar = FALSE)

    sim_sum <- .habVals %>%
      purrr::map(., ~ hab_sim(., p_terrs, pb= prog)) %>%
      dplyr::bind_rows() %>%
      dplyr::mutate(sim_num = .simNum)

  }

  sim_df <- n_terr_sims %>%
    purrr::map(., ~ terr_sim(., BeaverNetwork, hab_mins, pb = progbar, target = length(n_terr_sims))) %>%
    dplyr::bind_rows()

}

