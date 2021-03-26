
terr_checks <- function(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh, att){

  if (terr_leng > (t_length*(1-thresh)) && terr_leng < (t_length*(1+thresh))){

    s_ord <- dplyr::pull(reach, var='Str_order')[1]

    terr_line <- terr_line %>%
      dplyr::mutate(Str_Ord = s_ord) %>%
      dplyr::mutate(Terr_Leng =terr_leng)
    return(terr_line)

  } else {

    if (is.null(new_buff)){
      if (terr_leng < t_length){
        recalc_buff <- buff * 1.5
      } else {
        recalc_buff <- buff * 0.5
      }

    } else {
      if (terr_leng < t_length){
        recalc_buff <- buff + ((abs(old_buff-buff))/2)
      } else {
        recalc_buff <- buff - ((abs(old_buff-buff))/2)
      }

    }
    create_territories(reach, river, t_length, new_buff = recalc_buff, old_buff = buff, attempt = att)
  }

}


create_territories <- function(reach, river, t_length=NULL,  new_buff= NULL, old_buff=NULL, attempt=0) {
  s_ord <- reach %>%
    dplyr::pull(Str_order)

  river <- river %>%
    dplyr::filter(Str_order >= s_ord)

  if (is.null(t_length)){
    # Here the random territory size is generated - based on Literature - add details...
    # t_length <- rnorm(1, 1630, 293) # think this is wrong...
    t_length <-runif(1, min = 1630-293, max = 1630+293)
  }

  if (is.null(new_buff)){
    buff <- t_length/2


  } else {
    buff <- new_buff
  }

  terr_line <- reach %>%
    sf::st_buffer(buff) %>%
    sf::st_intersection(river) %>%
    dplyr::mutate(Leng = as.numeric(sf::st_length(.))) %>%
    sf::st_buffer(0.1) %>%
    dplyr::summarise(dplyr::across(c("BFI_40m", "BDC", "Str_order"),
                                   ~ weighted.mean(.x, w= Leng, na.rm = TRUE), .names = "mean_{.col}"))


  if (sf::st_geometry_type(terr_line)=='MULTIPOLYGON'){
    att <- attempt + 1
    terr_line <- sf::st_cast(terr_line, to= 'POLYGON') %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::filter(area == max(area))  %>%
      sf::st_intersection(river) %>%
      dplyr::mutate(Leng = as.numeric(sf::st_length(.))) %>%
      sf::st_buffer(0.1) %>%
      dplyr::summarise(across(c("BFI_40m", "BDC", "Str_order"),
                              ~ weighted.mean(.x, w= Leng, na.rm = TRUE), .names = "mean_{.col}"))


    terr_leng <- as.numeric(lwgeom::st_perimeter(sf::st_buffer(terr_line, 0.1)))/2

    if ( att >=3){
      stop('Cannot generate territory of accecptable size - discard for reach.')
    }

    #threshold of within 40% of target applied to allow adjustment of territories.
    # max and min will still fall within max and min values reported in literature.
    return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.1, att=att))

  }

  terr_leng <- as.numeric(lwgeom::st_perimeter(st_buffer(terr_line, 0.1)))/2

  return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.05, att=0))

}


#' Generate potential territories for all reaches of a BeaverNetwork dataset.
#'
#' Generates a theoretical territory area for every reach within the catchment. Reaches must be <1000m but <200m is recomended.
#' Territory sizes (based on length of channel) are randomly generated from a normal distribution using `rnorm(1, 1630, 293)`
#' where the mean is 1630 with a standard deviation of 293m. This range is based on empiracal studies:
#' [Campbell, et al. (2005)](https://link.springer.com/article/10.1007/s00265-005-0942-6#citeas);
#' [Vorel, et al. (2008)](https://hrcak.srce.hr/index.php?id_clanak_jezik=53979&show=clanak),
#' [John and Kostkan (2009)](https://www.researchgate.net/profile/Vlastimil_Kostkan/publication/228683750_Compositional_analysis_and_GPSGIS_for_study_of_habitat_selection_by_the_European_beaver_Castor_fiber_in_the_middle_reaches_of_the_Morava_River/links/53db8cd00cf2cfac9928ef55.pdf),
#' [Graf, et al. (2016)](https://link.springer.com/article/10.1016/j.mambio.2016.07.046); and
#' [Mayer, et al. (2017)](https://onlinelibrary.wiley.com/doi/full/10.1002/ece3.2988).
#'
#' @param BeaverNetwork A river network with attributed results from Graham, et al., (2020) or Macfarlane, et al., (2017)
#' An sf object or an sf-readable file. See sf::st_drivers() for available drivers.
#' @param progbar Boolean to use a progress bar to monitor progress
#' @param multicore  Boolean to multiple core - This function can be slow for large catchments so TRUE is recomended.
#' @param ncores numeric denoting the number of processes to run the function across. If not included, defaults to:
#' `parallel::detectCores()[1]-2`
#' @return an 'sf object containing a potential territory area for every reach from the input BeaverNetwork.
#' @import foreach doParallel parallel tcltk
#' @export
#' @examples
#' # here we read in the BeaverNetwork data
#' # NOTE - MUST ADD OPEN SOURCE VERSION AS BUILT IN DATA ASAP!
#' BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg')
#'
#' # ---------- Subset dataset for example to reduce computation time -----------
#' BeavNetOtter <- BeavNetOtter[BeavNetOtter$Str_order > 3,]
#'
#' # ---------- run terriroty generation --------
#' test_out <-  gen_territories(BeavNetOtter)
#'
gen_territories <- function(BeaverNetwork, progbar=TRUE, multicore=TRUE, ncores){
  # silence warnings...
  oldw <- getOption("warn")
  options(warn = -1)

  # function to safely call the territory generation:
  gen_terr_safe <- function(x, it, pbar) {

    if (!is.null(pbar)){
      tcltk::setTkProgressBar(pbar, it)
    }

    f = purrr::safely(function() create_territories(reach = x, river = BeaverNetwork))

    f()

  }

  # function to generate tcltk progress bar.
  create_progbar <- function(n){
    if (isTRUE(progbar)){
      if(!exists("counter")) counter <- 0
      counter <- counter + 1
      if(!exists("pb")) pb <- tcltk::tkProgressBar("Generating potential territories", min=1, max=n)
    } else {
      pb <- NULL
    }
    return(pb)
  }

  #setup parallel backend to use many processors
  if (isFALSE(multicore)){
    # ncores <- 1

    pb <- create_progbar(n=nrow(BeaverNetwork))

    out <- BeaverNetwork %>%
      dplyr::mutate(id = dplyr::row_number()) %>%
      dplyr::group_by(id) %>%
      dplyr::group_split() %>%
      purrr::imap( ~ gen_terr_safe(x = .x, it = .y, pbar = pb)) %>%
      purrr::map(., ~ .$result) %>%
      dplyr::bind_rows()

    if (!is.null(pb)){
      close(pb)
    }

  } else {
    if (missing(ncores)){
      cores=parallel::detectCores()[1]-2
    } else {
      cores <- ncores
    }

    cl <- parallel::makeCluster(cores)

    doParallel::registerDoParallel(cl)

    split_df <- BeaverNetwork %>%
      dplyr::group_by((dplyr::row_number()-1) %/% (dplyr::n()/cores)) %>%
      dplyr::group_split()

    # run paralell territory generation...
    out <- foreach::foreach(i = seq_along(split_df), .combine = rbind,
                            .packages = c("dplyr", "sf", "purrr", "lwgeom", "magrittr", "tcltk"),
                            .export = c("gen_terr_safe", "create_territories", "terr_checks")) %dopar% {


                              if (i == 1) {
                                n <- nrow(split_df[[i]])
                                pb <- create_progbar(n=n)
                                # if (isTRUE(progbar)){
                                #   n <- nrow(split_df[[i]])
                                #   if(!exists("counter")) counter <- 0
                                #   counter <- counter + 1
                                #   if(!exists("pb")) pb <- tcltk::tkProgressBar("Generating potential territories", min=1, max=n)
                                # } else {
                                #   pb <- NULL
                                # }

                              } else {
                                pb <- NULL
                              }


                              par_terrs <- split_df[[i]] %>%
                                dplyr::mutate(id = dplyr::row_number()) %>%
                                dplyr::group_by(id) %>%
                                dplyr::group_split() %>%
                                purrr::imap( ~ gen_terr_safe(x = .x, it = .y, pbar = pb)) %>%
                                purrr::map(., ~ .$result) %>%
                                dplyr::bind_rows()

                              return(par_terrs)
                            }

    parallel::stopCluster(cl)
  }



  #enable warnings
  options(warn = oldw)

  out <- out %>%
    dplyr::mutate(id = dplyr::row_number())

  imp_terr_n <- nrow(BeaverNetwork) -nrow(out)

  if (imp_terr_n > 0) {
    warning(sprintf('Territories could not be generated for %s reaches',
                    imp_terr_n))
  }


  return(out)
}


