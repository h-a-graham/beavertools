
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
#' @import foreach doParallel parallel progress
#' @export
gen_territories <- function(BeaverNetwork){
  oldw <- getOption("warn")
  options(warn = -1)

  #setup parallel backend to use many processors

  # cores <- 10
  cores=parallel::detectCores()
  cl <- parallel::makeCluster(cores[1]-2) #not to overload your computer

  cl <- parallel::makeCluster(cores, cores = cores)

  doParallel::registerDoParallel(cl)

  iter_size <- nrow(BeaverNetwork)


  # iter_size= 100
  n <- ceiling(iter_size/cores)


  # st <- seq(1,iter_size,by=n)
  # en <- c(seq(n,iter_size,by=n),iter_size)

  split_df <- BeaverNetwork %>%
    dplyr::group_by((dplyr::row_number()-1) %/% (dplyr::n()/cores)) %>%
    dplyr::group_split()

  # pb <- progress::progress_bar$new(total = nrow(BeaverNetwork),
  #                                  clear = FALSE)

  gen_terr_safe <- function(x) {
    # pb$tick()

    f = purrr::safely(function() create_territories(reach = x, river = BeaverNetwork))

    f()

  }
  # run paralell territory generation...
  out <- foreach::foreach(i = seq_along(split_df), .combine = rbind,
                         .packages = c("dplyr", "sf", "purrr", "lwgeom", "tibble", "magrittr"),
                         .export = c("gen_terr_safe", "create_territories", "terr_checks")) %dopar% {





                           par_terrs <- split_df[[i]] %>%
                             tibble::rowid_to_column(., var='id') %>%
                             dplyr::group_by(id) %>%
                             dplyr::group_split() %>%
                             purrr::map( ~ gen_terr_safe(.)) %>%
                             purrr::map(., ~ .$result) %>%
                             dplyr::bind_rows()

                           return(par_terrs)
                         }

  parallel::stopCluster(cl)

  options(warn = oldw)

  out <- out %>%
    tibble::rowid_to_column(., var='id')

  imp_terr_n <- nrow(BeaverNetwork) -nrow(out)

  if (imp_terr_n > 0) {
    warning(sprintf('Territories could not be generated for %s reaches',
                    imp_terr_n))
  }


  return(out)
}


