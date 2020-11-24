terr_checks <- function(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh, att){

  if (terr_leng > (t_length*(1-thresh)) && terr_leng < (t_length*(1+thresh))){

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
    create_territories(reach, river, t_length, progbar=NULL, new_buff = recalc_buff, old_buff = buff, attempt = att)
  }

}


create_territories <- function(reach, river, t_length, progbar=NULL, new_buff= NULL, old_buff=NULL, attempt=0) {


  # if (!is.null(progbar)){
  #   progbar$tick()
  # }

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


  if (st_geometry_type(terr_line)=='MULTIPOLYGON'){
    att <- attempt + 1
    terr_line <- sf::st_cast(terr_line, to= 'POLYGON') %>%
      dplyr::mutate(area = sf::st_area(.)) %>%
      dplyr::filter(area == max(area))  %>%
      sf::st_intersection(river) %>%
      dplyr::mutate(Leng = as.numeric(sf::st_length(.))) %>%
      dplyr::summarise(across(c("BFI_40m", "BDC"), ~ weighted.mean(.x, w= Leng, na.rm = TRUE)))%>%
      sf::st_buffer(0.1)

    terr_leng <- as.numeric(lwgeom::st_perimeter(st_buffer(terr_line, 0.1)))/2

    if ( att >=5){
      stop('Cannot generate territory of accecptable size - discard for reach.')
    }

    return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.4, att=att))


    # if (terr_leng > (t_length*0.6) && terr_leng < (t_length*1.4)){
    #   return(terr_line)
    # } else {
    #   create_territories(reach, river, t_length, progbar=NULL, new_buff = recalc_buff, old_buff = buff, attempt = att)
    # }

  }

  terr_leng <- as.numeric(lwgeom::st_perimeter(st_buffer(terr_line, 0.1)))/2
  # message(terr_leng)

  return(terr_checks(reach, river, terr_line,  terr_leng, t_length, buff, old_buff, new_buff, thresh = 0.05, att=0))
  # if (terr_leng > (t_length*0.95) && terr_leng < (t_length*1.05)){
  #
  #   return(terr_line)
  #
  # } else {
  #
  #   if (is.null(new_buff)){
  #     if (terr_leng < t_length){
  #       recalc_buff <- buff * 1.5
  #     } else {
  #       recalc_buff <- buff * 0.5
  #     }
  #
  #   } else {
  #     if (terr_leng < t_length){
  #       recalc_buff <- buff + ((abs(old_buff-buff))/2)
  #     } else {
  #       recalc_buff <- buff - ((abs(old_buff-buff))/2)
  #     }
  #
  #   }
  #   create_territories(reach, river, t_length, progbar=NULL, new_buff = recalc_buff, old_buff = buff)
  # }

}

gen_terr_safe <- function() {
  progbar$tick()

    purrr::safely(create_territories(., BeaverNetwork, 1000, progbar = pb))

}



#' recursive function to create territory channel length to within 5% of target
#'
#' @export
gen_territories <- function(BeaverNetwork){
  pb <- progress::progress_bar$new(total = nrow(BeaverNetwork),
                                   clear = FALSE)

  out <- BeaverNetwork %>%
    tibble::rowid_to_column(., var='id') %>%
    group_by(id) %>%
    group_split() %>%
    purrr::map(., ~ purrr::safely(create_territories(., BeaverNetwork, 1000, progbar = pb))) #%>%
    #dplyr::bind_rows()
  print('BREAK')
}


