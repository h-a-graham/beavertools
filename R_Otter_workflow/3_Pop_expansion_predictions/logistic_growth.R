# Function to fit the logistic growth model.


logistic_growth <- function(cap_val, .logmodel, st.date=2007){
  # get half capacity prediction date...
  HalfCap <- function(cap){
    yearsVal <- tibble(years_since = seq(9,50, by=0.01)) %>%
      broom::augment(.logmodel, newdata=., se_fit=T, type.predict = "response",
                     type.residuals = "deviance") %>%
      mutate(.fitround = floor(.fitted)) %>%
      filter(.fitround==round(cap/2)) %>%
      pull(years_since)
    yearsVal[1]
  }

  #create new df with anchor points.
  hacked_df_func <- function(cap, st.date){
    terr_counts %>%
      sf::st_drop_geometry() %>%
      # bind_rows(tibble(terr_count=round(cap)/2, season= "Half Cap",
      #                  years_since=HalfCap(cap), year_adj=years_since + 2007)) %>%
      bind_rows(tibble(terr_count=round(cap), season="Full Cap",
                       years_since=HalfCap(cap) * 2 , year_adj=years_since + st.date))

  }

  # function to fit new spline
  fit_n_predict <- function(df, cap, st.date){
    .logistic_model <- nls(terr_count ~ SSlogis(years_since, Asym, xmid, scal),df)

    # create new data with predictions
  new_data <- tibble(years_since = seq(8,50, by=0.5))

  # this method produces identical CIs to below but much slower
  # p <- propagate::predictNLS(.logistic_model, newdata = new_data, interval='confidence')
  # predictions <- new_data %>%
  #     mutate(year_adj = years_since + 2007,
  #            cap_name = cap,
  #            .fitted = p$summary$Sim.Mean,
  #            conf.lwr = p$summary$`Sim.2.5%`,
  #            conf.upr = p$summary$`Sim.97.5%`)

  predictions <- as_tibble(investr::predFit(.logistic_model, newdata = new_data,
                                            interval = "confidence", level= 0.95,
                                            adjust="Scheffe")) %>%
    rename(.fitted = fit,
           pred.lwr = lwr,
           pred.upr = upr)  %>%
    mutate(pred.lwr = ifelse(pred.lwr<0, 0, pred.lwr),
           years_since = new_data$years_since,
           year_adj = years_since + st.date,
           cap_name = cap,
           lwr_diff = pred.lwr/.fitted,
           upr_diff = pred.upr/.fitted,
           growth_rate = lead(.fitted)/.fitted,
           growth_rate.lwr = growth_rate*lwr_diff,
           growth_rate.upr = growth_rate*upr_diff,
           # growth_rate.lwr = lead(pred.lwr)/pred.lwr,
           # growth_rate.upr = lead(pred.upr)/pred.upr,
           hartman_rate = (lead(.fitted)/.fitted)/years_since,
           hartman_rate =  ifelse(hartman_rate<=1, hartman_rate, NA),
           hartman_rate.lwr = hartman_rate*lwr_diff,
           hartman_rate.upr = hartman_rate*upr_diff,
           # hartman_rate.lwr = (lead(pred.lwr)/pred.lwr)/years_since,
           # hartman_rate.upr = (lead(pred.upr)/pred.upr)/years_since,
           n_terr_growth = lead(.fitted)-.fitted,
           n_terr_growth.lwr = n_terr_growth*lwr_diff,
           n_terr_growth.upr = n_terr_growth*upr_diff)
           # n_terr_growth.lwr = lead(pred.lwr)-pred.lwr,
           # n_terr_growth.upr = lead(pred.upr)-pred.upr)

  return(predictions)




  #   new_data <- tibble(years_since = seq(0,50, by=0.01)) %>%
  #     broom::augment(.logistic_model, newdata=.) %>%
  #     mutate(year_adj = years_since + 2007,
  #            cap_name = cap)
  }


  hacked_df_func(cap_val, st.date) %>%
    fit_n_predict(., cap_val, st.date)
}
