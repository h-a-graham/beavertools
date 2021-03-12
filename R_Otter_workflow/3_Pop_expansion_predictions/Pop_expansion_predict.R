library(patchwork)
library(sf)
library(magrittr)
library(tidyverse)
library(patchwork)
library(ggfortify)
devtools::load_all()

#----- Define some directories -----------------

plot_dir <- file.path(here::here(),"R_Otter_workflow/3_Pop_expansion_predictions/plots")

# ---- Read in Data Territroy count data --------------
reclass_terr_list <- readRDS(file='R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list.Rds')
# get unique names for survey years...
plot_names <- unique(RivOtter_FeedSigns$SurveySeason)

# What is the capacity of the catchment? WILL NEED UPDATING WITH RANGES ETC WHEN SIMULATIONS ARE DONE.
cap_limits <- read_rds(file.path(export_dir, 'sim_terr.Rds')) %>%
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

## ----- uncomment for model diagnosticss. -------
# autoplot(.logmodel, which = 1:6, label.size = 3)
# summary(.logmodel)
# broom::tidy(.logmodel, exponentiate=T, conf.int=T)

# ----- functions to fit sigmoidal growth model -------
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
    bind_rows(tibble(terr_count=round(cap), season="Full Cap",
                     years_since=HalfCap(cap) * 2, year_adj=years_since + 2007))%>%
    bind_rows(tibble(terr_count=round(cap), season="Future Full Cap",
                     years_since=HalfCap(cap) * 2 + 50, year_adj=years_since + 2007))

}


# function to fit new spline
fit_n_predict <- function(df, cap){
  # fit new model
  .splinemod <- glm(terr_count ~ splines::ns(years_since,df=1, knots=c(HalfCap(cap)),
                                             Boundary.knots=c(0, HalfCap(cap)*2)),
                    family=poisson(link='log'), data=df)

  # create new data with predictions
  new_data <- tibble(years_since = seq(0,50, by=0.1)) %>%
    broom::augment(.splinemod, newdata=., se_fit=T, type.predict = "response",
                   type.residuals = "deviance") %>%
    mutate(conf.low = .fitted - (.se.fit*1.96),
           conf.high = .fitted + (.se.fit*1.96)) %>%
    mutate(
      .fitted = ifelse(.fitted>cap, cap, .fitted),
           # conf.low = ifelse(conf.low < 0, 0, conf.low),
           conf.high = ifelse(conf.high > cap, cap, conf.high),
           year_adj = years_since + 2007,
           cap_name = cap)
}

model_capacity <- function(cap_val){
  hacked_df_func(cap_val) %>%
    fit_n_predict(., cap_val)
}


hacked_df <-  seq(lower_capacity, upper_capacity, by=1) %>%
  purrr::map(., ~model_capacity(.)) %>%
  bind_rows()

ribbon_df <- tibble(year_adj = seq(2000,2070, by=70),
                    .fitted = seq(0,200, by=200))

hacked_df %>%
  ggplot(., aes(x=year_adj, y=.fitted))+
  geom_ribbon(data=ribbon_df, aes(ymin=lower_capacity, ymax=upper_capacity, xmin=2000),
              fill='grey70', size=0.1, alpha=0.3, linetype=2, color="grey20") +
  # geom_hline(yintercept=lower_capacity, linetype=1, lwd=0.1)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group=cap_name),
              alpha=0.04, lwd=NA, fill="#4daf4a") +
  stat_summary(aes(y=conf.high),fun = max, geom = 'line', size=0.1, alpha=0.6, linetype=2, color="grey20") +
  stat_summary(aes(y=conf.low),fun = min, geom = 'line', size=0.1, alpha=0.6, linetype=2, color="grey20") +
  stat_summary(fun = mean, geom = 'line', size=0.6, alpha=0.6, linetype=1, color="grey20") +
  geom_point(data=terr_counts, aes(x=year_adj, y=terr_count), inherit.aes = F, alpha=0.6)+
  annotate("text", x=2015, y = lower_capacity + ((upper_capacity-lower_capacity)*0.5),
           label = "Predicted territory capacity range", size=3) +
  # annotate("text", x=2015, y = upper_capacity + 5, label = "max predicted territory capacity", size=3) +

  # geom_hline(yintercept=upper_capacity, linetype=1, lwd=0.1)+
  coord_cartesian(ylim=c(0,upper_capacity +10), xlim = c(2007, 2050))+
  labs(x = 'year', y="n territories", subtitle="Projected beaver territory expansion in R. Otter Catchment")+
  theme_bw() +
  theme(legend.position = "bottom") +
  ggsave(file.path(plot_dir, 'TerritoryPredictionc.png'),
          dpi=300, height=7, width=7)






#==========================#
#===========SS=============#
#==========================#

# terr_pred <- ggplot(hacked_df, aes(x=year_adj, y=.fitted, colour=cap_name, fill=cap_name))+
#   geom_hline(yintercept=lower_capacity, linetype=4, lwd=0.3)+
#   geom_hline(yintercept=upper_capacity, linetype=4, lwd=0.3)+
#   # geom_line(data=new_data, aes(y = .log.fitted), colour='#000E54', lwd=1.2, linetype=3) +
#   geom_line(lwd=1.2) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
#               alpha=0.2, linetype=2, lwd=0.2) +
#   geom_point(data=terr_counts, aes(x=year_adj, y=terr_count), inherit.aes = F, alpha=0.8)+
#   # stat_smooth(method="lm",formula='y ~ poly(x, 2)', fullrange=TRUE, colour='#43B1FF', se=F) +
#   # geom_smooth(method = "glm", formula = y~x,
#   #               method.args = list(family = poisson(link = 'log')), fullrange=TRUE) +
#
#   # geom_vline(xintercept=HalfCap, linetype=4, lwd=0.3)+
#   annotate("text", x=2015.5, y = lower_capacity + 5, label = "Predicted territory capacity: Open Network", size=3) +
#   annotate("text", x=2015, y = upper_capacity + 5, label = "Predicted territory capacity: MM Network", size=3) +
#   scale_fill_manual(values = c("#d95f02", "#7570b3"), name=NULL)+
#   scale_colour_manual(values = c("#d95f02", "#7570b3"), name=NULL)+
#   # xlim(lubridate::dmy(c("30-12-2007", "30-12-2050"))) +
#   # xlim(0,100) +
#   # ylim(0,1000) +
#   # coord_cartesian(ylim=c(0,100), xlim = c(0,50))+
#   coord_cartesian(ylim=c(0,200), xlim = c(2007, 2050))+
#   # coord_cartesian(ylim=c(0,100), xlim = lubridate::dmy(c("30-12-2007", "30-12-2031")))+
#   labs(x = 'year', y="n territories", subtitle="Beaver territory expansion in R. Otter Catchment???")+
#   theme_bw() +
#   theme(legend.position = "bottom")
# terr_pred
# ggsave(file.path(plot_dir, 'Territory_predictionMMvsORN.png'),
#        plot = terr_pred, dpi=300, height=7, width=7)
