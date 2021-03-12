library(tidyverse)
devtools::load_all()

terr_list <- readRDS(file='R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list.Rds')

# MasterMap River Network not released with the package due to licensing issues.
MMRN_BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg') # MasterMap Data

# Survey season names ----

survey_years <- unique(RivOtter_FeedSigns$SurveySeason)

# load terr_BFI function
source(file.path(here::here(), 'R_Otter_workflow/2_Territory_simulations/terr_BFI_df.R'))

Terr_df <- terr_BFI_df(terr_list, MMRN_BeavNetOtter, survey_years)

ggplot(Terr_df, aes(fill=mean_BFI_40m ))+
  geom_sf() +
  facet_wrap(~survey_year, ncol=4) +
  coord_sf(crs = sf::st_crs(Terr_df), datum =  sf::st_crs(Terr_df)) +
  scale_fill_viridis_b()+
  ggplot2::theme(axis.title.x=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.ticks.x=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.ticks.y=ggplot2::element_blank(),
                 legend.position = "bottom") +
  guides(fill = guide_colourbar(barheight = 0.5))


ggplot(Terr_df, aes(x=mean_BFI_40m, colour = user_class, fill=user_class, after_stat(count))) +
  # geom_histogram(aes(group=user_class),bins = 5, alpha=0.4) +
  geom_density( fill=NA) +
  facet_wrap(~survey_year) +
  scale_fill_brewer(palette = "Dark2")+
  scale_colour_brewer(palette = "Dark2")+
  theme_bw() +
  theme(legend.position = "bottom")


message(sprintf("minimum BFI value: %s \nmean BFI value: %s",
                round(min(Terr_df$mean_BFI_40m),1), round(mean(Terr_df$mean_BFI_40m), 1)))



