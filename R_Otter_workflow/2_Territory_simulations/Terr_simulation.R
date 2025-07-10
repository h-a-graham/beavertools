# ------------ imports --------------------
# devtools::install()
library(beavertools)
library(tidyverse)
library(sf)
library(broom)
library(patchwork)

#-------------define folder root -------------

export_dir <- file.path(
  here::here(),
  "R_Otter_workflow/2_Territory_simulations/exports"
)
plot_dir <- file.path(
  here::here(),
  "R_Otter_workflow/2_Territory_simulations/plots"
)
# ----- generate extent and titles for mapping ------
target_ext <- inflate_bbox(RivOtter_Catch_Area, 10)
# ------------ Read Data ------------------

Real_terrs <- readRDS(
  'R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list.Rds'
)

# MasterMap River Network not released with the package due to licensing issues.
# MMRN_BeavNetOtter <- sf::read_sf('run/data/BeaverNetwork_Otter.gpkg') # MasterMap Data - used in publication.
# contact Hugh Graham, the lead authour of the package/publication or submit an issue here:

# OS Open Rivers Network - included with the package
MMRN_BeavNetOtter <- RivOtter_BeaverNet

#------- Observed territory Habitat stats ----------
# need to calculate some stats here and find out what kind of BFI values we're currently seeing on the Otter.
terr_list <- readRDS(
  file = file.path(
    here::here(),
    'R_Otter_workflow/1_Feed_Sign_Mapping/exports/reclass_terr_list2.Rds'
  )
)
survey_years <- RivOtter_FeedSigns %>%
  filter(SurveySeason != "Pre 2015") %>%
  pull(SurveySeason) %>%
  unique()

source(file.path(
  here::here(),
  'R_Otter_workflow/2_Territory_simulations/terr_BFI_df.R'
))

Terr_df <- terr_BFI_df(terr_list, MMRN_BeavNetOtter, survey_years)

lower_BFI <- round(min(Terr_df$mean_BFI_40m), 1)
upper_BFI <- round(mean(Terr_df$mean_BFI_40m), 1)

message(sprintf(
  "Lower BFI threshold: %s \nUpper BFI threshold: %s",
  lower_BFI,
  upper_BFI
))

# ---------- run terriroty generation for all reaches for each network
run_terr_gen <- function(riv_network, overwrite = FALSE, save_out = TRUE) {
  fileName <- file.path(
    export_dir,
    paste('PT_', deparse(substitute(riv_network)), '.Rds', sep = "")
  )
  # print(fileName)
  if (file.exists(fileName) && isFALSE(overwrite)) {
    message(sprintf("Potential territrories created in: %s", fileName))
    message("Loading previously generated file...")
    terr_out <- readRDS(fileName)
  } else {
    t1 <- Sys.time()
    terr_out <- gen_territories(riv_network, multicore = T, progbar = T)
    if (isTRUE(save_out)) {
      saveRDS(terr_out, file = fileName)
    }
    message(sprintf(
      'Potential Territory Generation Run Time = %s minutes',
      round(Sys.time() - t1, 1)
    ))
  }
  return(terr_out)
}

RivOtter_Terrs <- run_terr_gen(MMRN_BeavNetOtter)

(nrow(MMRN_BeavNetOtter) - nrow(RivOtter_Terrs)) / nrow(MMRN_BeavNetOtter) * 100

# terrs_union <- sf::st_union(MMRN_BeavNetOtter) %>%
#   st_as_sf()

#plot to check that whole network is cvered by the theoretical territories.
# ggplot()+
#   geom_sf(MMRN_BeavNetOtter, mapping= aes(), colour='red')+
#   geom_sf(terrs_union, mapping= aes(), colour='blue') +
#
#   theme_bw()

# ggplot(RivOtter_Terrs, aes(x=Terr_Leng))+
#   geom_density()

# ------------- Run territory cap -------------
run_terr_cap <- function(pot_terrs, veg, overwrite = FALSE, save_out = TRUE) {
  fileName <- file.path(
    export_dir,
    paste('TC_', deparse(substitute(pot_terrs)), '.Rds', sep = "")
  )
  # print(fileName)
  if (file.exists(fileName) && isFALSE(overwrite)) {
    message(sprintf("Territory capacity created in: %s", fileName))
    message("Loading previously generated file...")
    cap_out <- readRDS(fileName)
  } else {
    t1 <- Sys.time()
    cap_out <- territory_cap(pot_terrs, min_veg = veg, multicore = TRUE)
    if (isTRUE(save_out)) {
      saveRDS(cap_out, file = fileName)
    }
    message(sprintf(
      'Territory Capacity Run Time = %s minutes',
      round(Sys.time() - t1, 1)
    ))
  }
  return(cap_out)
}

RivOtter_Terrs_upper <- RivOtter_Terrs

terr_cap_lowBFI <- run_terr_cap(RivOtter_Terrs, lower_BFI, overwrite = F)
terr_cap_uppBFI <- run_terr_cap(RivOtter_Terrs_upper, upper_BFI, overwrite = F)

Terr_Cap_df <- terr_cap_lowBFI %>%
  mutate(sim = as.character(lower_BFI)) %>%
  bind_rows(
    .,
    terr_cap_uppBFI %>%
      mutate(sim = as.character(upper_BFI))
  )

# plot showing the territory length distributions for each of these example simulations.
tl_dens <- ggplot(
  Terr_Cap_df,
  aes(x = Terr_Leng, fill = sim, after_stat(count))
) +
  geom_density(alpha = 0.5) +
  labs(x = 'territory length (m)', fill = 'minimum BFI threshold') +
  scale_fill_brewer(palette = "Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(
  filename = file.path(plot_dir, 'TerritoryLengthDist.png'),
  plot = tl_dens,
  dpi = 600,
  height = 180,
  width = 180,
  units = 'mm'
)

# summary stats for territory length if needed
Terr_sum_df <- Terr_Cap_df %>%
  sf::st_drop_geometry() %>%
  group_by(sim) %>%
  summarise(
    mean_length = mean(Terr_Leng),
    stdev_Length = sd(Terr_Leng),
    n_terrs = n()
  )

# ---------- Plot Territory Capacities for both Networks ---------

capacity_plot <- function(cap_lowBFI, cap_uppBFI) {
  p1 <- plot_capacity(
    cap_uppBFI,
    buffer = 50,
    basemap = F,
    catchment = RivOtter_Catch_Area,
    river_net = MMRN_BeavNetOtter,
    plot_extent = target_ext,
    north_arrow = F,
    scalebar = F,
    axes_units = F,
    mask_fill = 'grey80'
  ) +
    annotate(
      "text",
      x = 304500,
      y = 81000,
      size = 2.4,
      label = sprintf(
        'n territories = %s',
        Terr_sum_df$n_terrs[Terr_sum_df$sim == as.character(upper_BFI)]
      )
    ) +
    annotate("text", x = 302000, y = 117000, size = 5, label = "A") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  p2 <- plot_capacity(
    cap_lowBFI,
    buffer = 50,
    basemap = F,
    catchment = RivOtter_Catch_Area,
    river_net = MMRN_BeavNetOtter,
    plot_extent = target_ext,
    axes_units = FALSE,
    mask_fill = 'grey80'
  ) +
    annotate(
      "text",
      x = 304500,
      y = 81000,
      size = 2.4,
      label = sprintf(
        'n territories = %s',
        Terr_sum_df$n_terrs[Terr_sum_df$sim == as.character(lower_BFI)]
      )
    ) +
    annotate("text", x = 325000, y = 117000, size = 5, label = "B") +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  p3 <- p1 +
    p2 +
    plot_annotation(
      caption = 'Contains: OS data © Crown copyright and database right 2021', #© OpenStreetMap contributors,
      theme = theme(plot.caption = element_text(size = 6))
    )

  return(p3)
}

TerrCapPlot <- capacity_plot(terr_cap_lowBFI, terr_cap_uppBFI)
ggsave(
  filename = file.path(plot_dir, 'Lower_Upper_Capacity_maps.png'),
  plot = TerrCapPlot,
  dpi = 300,
  height = 138,
  width = 180,
  units = 'mm'
)


# ------- Run Territory simulation based on the desired upper and lower  minimum BFI requirements. -----------

run_terr_simulation <- function(fileName, overwrite = FALSE) {
  if (file.exists(fileName) && isFALSE(overwrite)) {
    message(sprintf("Simulation results already generated. See: %s", fileName))
    message("Loading previously generated file...")
    sim_terr <- readRDS(fileName)
  } else {
    t1 <- Sys.time()
    sim_terr <- sim_terr_cap(
      MMRN_BeavNetOtter,
      n_p_terr_sim = 100,
      n_hab_sim = 2,
      min_veg = c(lower_BFI, upper_BFI)
    )
    saveRDS(sim_terr, fileName)
    Sys.time() - t1
    return(sim_terr)
  }
}

sim_terr <- run_terr_simulation(
  file.path(export_dir, 'sim_terr.Rds'),
  overwrite = F
)

# plot simulation results... load again if needed.
st <- sim_terr %>%
  mutate(min_BFI_c = as.character(min_BFI)) %>%
  ggplot(., aes(x = min_BFI_c, y = n, fill = min_BFI_c)) +
  geom_point(
    shape = 21,
    alpha = 0.7,
    position = position_jitterdodge(
      jitter.width = 0.1
    ),
    stroke = 0.5,
    show.legend = F
  ) +
  geom_boxplot(show.legend = F, alpha = 0.5, width = 0.3, outlier.shape = NA) +
  scale_fill_brewer(palette = "Dark2", ) +
  coord_cartesian(y = c(100, 200)) +
  labs(y = "n territories", x = 'Beaver Forage Index (BFI) Value') +
  theme_bw()

ggsave(
  st,
  filename = file.path(plot_dir, 'SimulationResults1.png'),
  dpi = 600,
  height = 180,
  width = 180,
  units = 'mm'
)


sim_terr %>%
  sf::st_drop_geometry() %>%
  group_by(min_BFI) %>%
  summarise(.mean = mean(n), .stdev = sd(n), .min = min(n), .max = max(n))

# quick summary to retrieve the highest and lowest possible capacity values.
sim_terr %>%
  sf::st_drop_geometry() %>%
  summarise(lowest = min(n), highest = max(n))
