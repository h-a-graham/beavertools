
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

  if (is.null(t_length)){
    # Here the random territory size is generated - based on Literature - add details...
    t_length <- rnorm(1, 1630, 293)
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
    dplyr::summarise(across(c("BFI_40m", "BDC"), ~ weighted.mean(.x, w= Leng, na.rm = TRUE)))%>%
    sf::st_buffer(0.1)


  if (sf::st_geometry_type(terr_line)=='MULTIPOLYGON'){
    att <- attempt + 1
    terr_line <- sf::st_cast(terr_line, to= 'POLYGON') %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::filter(area == max(area))  %>%
      sf::st_intersection(river) %>%
      dplyr::mutate(Leng = as.numeric(sf::st_length(.))) %>%
      dplyr::summarise(across(c("BFI_40m", "BDC"), ~ weighted.mean(.x, w= Leng, na.rm = TRUE)))%>%
      sf::st_buffer(0.1)

    terr_leng <- as.numeric(lwgeom::st_perimeter(sf::st_buffer(terr_line, 0.1)))/2

    if ( att >=5){
      stop('Cannot generate territory of accecptable size - discard for reach.')
    }

    #threshold of within 40% of target applied to allow adjustment of territories.
    # max and min will still fall within max and min values reported in literature.
    return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.4, att=att))

  }

  terr_leng <- as.numeric(lwgeom::st_perimeter(st_buffer(terr_line, 0.1)))/2

  return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.05, att=0))

}


#' Caller function for create territories wrapped in safely.
#'
#' @import foreach doParallel parallel tcltk
#' @export
gen_territories <- function(BeaverNetwork, progbar=TRUE, multicore=TRUE, ncores){
  # silence warnings...
  oldw <- getOption("warn")
  options(warn = -1)

  #setup parallel backend to use many processors
  if (isFALSE(multicore)){
    ncores <- 1
  }

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


  gen_terr_safe <- function(x, it, progbar) {

    if (!is.null(progbar)){
      tcltk::setTkProgressBar(progbar, it)
    }

    f = purrr::safely(function() create_territories(reach = x, river = BeaverNetwork))

    f()

  }
  # run paralell territory generation...
  out <- foreach::foreach(i = seq_along(split_df), .combine = rbind,
                         .packages = c("dplyr", "sf", "purrr", "lwgeom", "magrittr", "tcltk"),
                         .export = c("gen_terr_safe", "create_territories", "terr_checks")) %dopar% {


                           if (i == 1) {
                             if (isTRUE(progbar)){
                               n <- nrow(split_df[[i]])
                               if(!exists("counter")) counter <- 0
                               counter <- counter + 1
                               if(!exists("pb")) pb <- tcltk::tkProgressBar("Generating potential territories", min=1, max=n)
                             } else {
                               pb <- NULL
                             }

                           } else {
                             pb <- NULL
                           }


                           par_terrs <- split_df[[i]] %>%
                             dplyr::mutate(id = dplyr::row_number()) %>%
                             dplyr::group_by(id) %>%
                             dplyr::group_split() %>%
                             purrr::imap( ~ gen_terr_safe(x = .x, it = .y, progbar = pb)) %>%
                             purrr::map(., ~ .$result) %>%
                             dplyr::bind_rows()

                           return(par_terrs)
                         }

  parallel::stopCluster(cl)

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


