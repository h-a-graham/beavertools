# Function to fit the logistic growth model.


logistic_growth <- function(cap_val, .logmodel){
  # get half capacity prediction date...
  HalfCap <- function(cap){
    yearsVal <- tibble(years_since = seq(0,50, by=0.01)) %>%
      broom::augment(.logmodel, newdata=., se_fit=T, type.predict = "response",
                     type.residuals = "deviance") %>%
      mutate(.fitround = floor(.fitted)) %>%
      filter(.fitround==round(cap/2)) %>%
      pull(years_since)
    yearsVal[1]
  }

  #create new df with anchor points.
  hacked_df_func <- function(cap){
    terr_counts %>%
      sf::st_drop_geometry() %>%
      bind_rows(tibble(terr_count=round(cap)/2, season= "Half Cap",
                       years_since=HalfCap(cap), year_adj=years_since + 2007)) %>%
      bind_rows(tibble(terr_count=round(cap), season=" Full Cap",
                       years_since=HalfCap(cap) * 2 , year_adj=years_since + 2007))

  }

  # function to fit new spline
  fit_n_predict <- function(df, cap){
    .logistic_model <- nls(terr_count ~ SSlogis(years_since, Asym, xmid, scal),df)

    # create new data with predictions
  new_data <- tibble(years_since = seq(0,50, by=1))

  # this method produces identical CIs to below but much slower
  # p <- propagate::predictNLS(.logistic_model, newdata = new_data, interval='confidence')
  # predictions <- new_data %>%
  #     mutate(year_adj = years_since + 2007,
  #            cap_name = cap,
  #            .fitted = p$summary$Sim.Mean,
  #            conf.lwr = p$summary$`Sim.2.5%`,
  #            conf.upr = p$summary$`Sim.97.5%`)

  predictions <- as_tibble(investr::predFit(.logistic_model, newdata = new_data, interval = "prediction", level= 0.95)) %>%
    rename(.fitted = fit,
           pred.lwr = lwr,
           pred.upr = upr)  %>%
    mutate(pred.lwr = ifelse(pred.lwr<0, 0, pred.lwr),
           years_since = new_data$years_since,
           year_adj = years_since + 2007,
           cap_name = cap,
           growth_rate = lead(.fitted)/.fitted,
           # growth_rate.lwr = lead(pred.lwr)/pred.lwr,
           # growth_rate.upr = lead(pred.upr)/pred.upr,
           hartman_rate = (lead(.fitted)/.fitted)/years_since,
           hartman_rate =  ifelse(hartman_rate<=1, hartman_rate, NA),
           # hartman_rate.lwr = (lead(pred.lwr)/pred.lwr)/years_since,
           # hartman_rate.upr = (lead(pred.upr)/pred.upr)/years_since,
           n_terr_growth = lead(.fitted)-.fitted)
           # n_terr_growth.lwr = lead(pred.lwr)-pred.lwr,
           # n_terr_growth.upr = lead(pred.upr)-pred.upr)

  return(predictions)




  #   new_data <- tibble(years_since = seq(0,50, by=0.01)) %>%
  #     broom::augment(.logistic_model, newdata=.) %>%
  #     mutate(year_adj = years_since + 2007,
  #            cap_name = cap)
  }


  hacked_df_func(cap_val) %>%
    fit_n_predict(., cap_val)
}
