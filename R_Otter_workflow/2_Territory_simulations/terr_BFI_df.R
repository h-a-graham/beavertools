
# function to return the mean and min BFI values of observed
# territories and possible territories to constrain simulations.

terr_BFI_df <- function(t_list, BeavNet, survey_names){


  hab_stats <- function(obs_terr, network){
    obs_terr %>%
      sf::st_intersection(network) %>%
      dplyr::mutate(Leng = as.numeric(sf::st_length(.))) %>%
      dplyr::summarise(dplyr::across(c("BFI_40m", "BDC", "Str_order"),
                                     ~ weighted.mean(.x, w= Leng, na.rm = TRUE), .names = "mean_{.col}"),
                       sum_BFI = mean(BFI_40m),
                       dplyr::across(c("BFI_40m", "BDC"),
                                     sum, .names = "sum_{.col}"),
                       dplyr::across(c("BFI_40m", "BDC"),
                                     max, .names = "sum_{.col}")) %>%
      sf::st_drop_geometry() %>%
      bind_cols(obs_terr,.)
  }

  safe_hab_stats <- function(obs_terr, network){
    f = purrr::safely(function() hab_stats(obs_terr=obs_terr, network=network))
    f()$result
  }


  CleanNcall <- function(Terrs, surv_name, net){
    Terrs%>%
      filter(user_class == 'Territory' |user_class == 'Possible' ) %>%
      mutate(id = row_number())%>%
      group_by(id) %>%
      group_split() %>%
      purrr::map(., ~safe_hab_stats(., net)) %>%
      bind_rows() %>%
      mutate(survey_year = surv_name)
  }

  Terr_df <- t_list %>%
    purrr::map2(.x=.,.y=survey_names, ~CleanNcall(.x,.y, BeavNet)) %>%
    bind_rows()

}



