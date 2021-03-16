library(patchwork)
library(sf)
library(magrittr)
library(tidyverse)
library(patchwork)
library(ggfortify)
devtools::load_all()

#----- Define some directories -----------------

plot_dir <- file.path(here::here(),"R_Otter_workflow/3_Pop_expansion_predictions/plots")
sim_dir <- file.path(here::here(),"R_Otter_workflow/2_Territory_simulations/exports")
# ---- Read in Data Territroy count data --------------
reclass_terr_list <- readRDS(file='R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list.Rds')
# get unique names for survey years...
plot_names <- unique(RivOtter_FeedSigns$SurveySeason)

# What is the capacity of the catchment? WILL NEED UPDATING WITH RANGES ETC WHEN SIMULATIONS ARE DONE.
cap_limits <- read_rds(file.path(sim_dir, 'sim_terr.Rds')) %>%
  sf::st_drop_geometry() %>%
  summarise(lowest = min(n),
            highest = max(n))
lower_capacity = cap_limits$lowest
upper_capacity = cap_limits$highest

#  ------- set up dataframe for observed territory counts... -------
date_list <- lubridate::dmy(c("30-12-2014", "30-12-2015", "30-12-2016",
                              "30-12-2017", "30-12-2018", "30-12-2019",
                              "30-12-2020"))
years_since_release <- c(8:14)

get_terr_counts <- function(terr_map, .season, .year, .ysr){
  terr_map %>%
    dplyr::filter(user_class == 'Territory') %>%
    summarise(terr_count = n(), season= .season, year = .year, years_since = .ysr)

}

count_obj_list <- list(reclass_terr_list, plot_names, date_list, years_since_release)
terr_counts <-  purrr::pmap(count_obj_list, ~get_terr_counts(..1, ..2, ..3, ..4)) %>%
  bind_rows() %>%
  mutate(year_adj = years_since + 2007) %>%
  mutate(terr_count = ifelse(season %in% c('2020 - 2021'), terr_count+1, terr_count )) # required because

# reclass_terr_list[[1]] %>% # for double checking only.
#   filter(user_class == "Territory") %>%
#   ggplot(., aes(fill=(as.character(id)))) +
#   geom_sf()

# --- fit log glm model to determine the fit up to 50% capacity ----------

.logmodel <- glm(terr_count ~ years_since, family=poisson(link='log'), data=terr_counts)

# --- visualise initial model ------
tibble(years_since  = seq(0,50, by=0.01)) %>%
  broom::augment(.logmodel, newdata=., se_fit=T, type.predict = "response") %>%
  ggplot(., aes(x=years_since, y = .fitted))+
  geom_point(data=terr_counts, aes(x=years_since, y=terr_count), inherit.aes = F, alpha=0.6)+
  geom_line() +
  geom_ribbon(aes(ymin=.fitted -(1.96*.se.fit),ymax=.fitted +(1.96*.se.fit) ), alpha=0.3)+
  coord_cartesian(ylim=c(0,upper_capacity +10))+
  theme_bw()


## ----- uncomment for model diagnosticss. -------
# autoplot(.logmodel, which = 1:6, label.size = 3)
# summary(.logmodel)
# broom::tidy(.logmodel, exponentiate=T, conf.int=T)


# ----- functions to fit logistic growth model -------

model_population <- function(cap_val){
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
    new_data <- tibble(years_since = seq(0,50, by=0.01)) %>%
      broom::augment(.logistic_model, newdata=.) %>%
      mutate(year_adj = years_since + 2007,
             cap_name = cap)
  }


  hacked_df_func(cap_val) %>%
    fit_n_predict(., cap_val)
}

# generate logistic models...
hacked_df <-  seq(lower_capacity, upper_capacity, by=1) %>%
  purrr::map(., ~model_population(.)) %>%
  bind_rows()

# function to create capcity ribbon for plot.
ribbon_df <- function() tibble(year_adj = seq(2000,2070, by=70),.fitted = seq(0,200, by=200))

mid_cap <- function() lower_capacity + ((upper_capacity-lower_capacity)*0.5)

# create plot.
hacked_df %>%
  ggplot(., aes(x=year_adj, y=.fitted))+
  geom_ribbon(data=ribbon_df(), aes(ymin=lower_capacity, ymax=upper_capacity, xmin=2000),
              fill='grey90', size=0.1, alpha=0.2, linetype=2, color="grey10") +
  geom_line(aes(group=cap_name),alpha=0.1, lwd=0.6, colour="#00B76F") +
  stat_summary(fun = mean, geom = 'line', size=0.6, alpha=0.6, linetype=1, color="grey20") +
  stat_summary(fun = min, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +
  stat_summary(fun = max, geom = 'line', size=0.4, alpha=0.6, linetype=2, color="grey20") +
  geom_point(data=terr_counts, aes(x=year_adj, y=terr_count), inherit.aes = F, alpha=0.6)+
  annotate("text", x=2020, y = mid_cap(),
           label = "predicted territory capacity range", size=3) +
  geom_segment(aes(x = 2020, y = mid_cap() + 5, xend = 2020, yend = upper_capacity -1),
               arrow = arrow(length = unit(0.01, "npc")),lwd=0.5, color="grey20") +
  geom_segment(aes(x = 2020, y = mid_cap() - 5, xend = 2020, yend = lower_capacity +1),
               arrow = arrow(length = unit(0.01, "npc")), lwd=0.5, color="grey20") +
  coord_cartesian(ylim=c(0,upper_capacity +10), xlim = c(2007, 2050))+
  labs(x = 'Year', y="Number of Territories")+
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(margin = margin(t = 0, r = 3, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 3, r = 0, b = 0, l = 0))) +
  ggsave(file.path(plot_dir, 'TerritoryPredictionc.png'),
          dpi=300, height=7, width=7)


