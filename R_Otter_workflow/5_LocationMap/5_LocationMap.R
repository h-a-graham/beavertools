#Location Map

library(beavertools)
library(raytrix)
library(rayshader)
library(tmap)
library(sf)
library(vapour)
library(cowplot)
library(dplyr)
library(viridisLite)
library(scico)
library(ggplot2)

catch <- beavertools::RivOtter_Catch_Area


set_canvas_sf(st_buffer(catch, 1500))
get_canvas(20)
tc <- topo_matrix(res = 20, src = 'aws')
ov <- map_drape(
  res = 10,
  src = 'wms_arcgis_mapserver_ESRI.WorldImagery_tms',
  alpha = 0.6
)

tex <- tc %>%
  sphere_shade(texture = 'bw') %>%
  add_overlay(ov, rescale_original = T) %>%
  add_shadow(., ray_shade(tc, zscale = 20, sunangle = 15, multicore = T))

# plot_map(tex)
tex_ras <- texture_to_brick(tex)

ott_mask <- st_difference(st_buffer(catch, 2e4), catch)


gb <- st_as_sf(geodata::gadm(
  "GB",
  path = ".",
  level = 1,
  version = "latest",
  resolution = 1
)) %>%
  st_union() %>%
  st_transform(27700)

region <- st_bbox(st_buffer(catch, 100)) %>%
  st_as_sfc() %>%
  st_set_crs(st_crs(catch))

rivs <- beavertools::get_rivers(catch) %>%
  mutate(name = 'River Network')

settles <- data.frame(
  x = c(309862, 316300),
  y = c(095402, 100689),
  place = c('Ottery St. Mary', 'Honiton')
) %>%
  st_as_sf(coords = c('x', 'y'), crs = 27700)


main <- tm_shape(tex_ras, raster.downsample = T) +
  tm_rgb(legend.show = FALSE) +
  tm_shape(ott_mask) +
  tm_fill(col = 'grey80', alpha = 0.7) +
  tm_shape(rivs) +
  tm_lines(col = '#9ED9FF', alpha = 0.9) +
  tm_compass(position = c("left", "top")) +
  tm_shape(settles) +
  tm_dots(size = 0.7, col = 'grey90') +
  tm_text('place', col = 'grey90', fontface = 'bold', ymod = 0.7) +
  tm_scale_bar(
    breaks = c(0, 2, 4),
    text.size = 0.8,
    position = c("left", "top")
  ) +
  tm_layout(
    legend.title.color = NA,
    legend.position = c(0.02, 0.75),
    legend.text.size = 1.5
  )


inset <- tm_shape(gb) +
  tm_polygons(col = 'white', alpha = 0.3) +
  tm_layout(frame = FALSE, bg.color = "transparent") +
  tm_shape(st_difference(st_buffer(region, 15000), region)) +
  tm_polygons(col = 'black', alpha = 0.8)


#make grobs
m <- tmap_grob(main)
i <- tmap_grob(inset)

#draw and save
im <- ggdraw() +
  draw_plot(m, width = 1.2, height = 1.2, x = -1, y = -1) +
  draw_plot(i, width = 0.5, height = 0.5, x = 0.4, y = 0.03) +
  draw_text(
    "Made with: © OpenStreetMap contributors, GADM country polygons,
GEBCO Compilation Group (2020) GEBCO 2020 Grid (doi:10.5285/a29c5465-b138-234d-e053-6c86abc040b9),
Tiles © Esri — Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community          ",
    x = 0.1,
    y = 0.03,
    size = 5,
    hjust = 0,
    vjust = 0.5
  )

ggsave(
  'R_Otter_workflow/5_LocationMap/map/loc_map.png',
  width = 14.5,
  height = 18,
  units = 'cm'
)

obn <- beavertools::RivOtter_BeaverNet
plot(obn[1])
# st_buffer(catch, 1500)
riv_plot <- function(
  .var,
  lrg.tit,
  style = "kmeans",
  dig = 1,
  col.func = rev(viridis(10)),
  ...
) {
  tm_shape(obn, bbox = st_buffer(catch, 1500)) +
    tm_lines(
      col = .var,
      alpha = 0.9,
      palette = col.func,
      lwd = 2,
      style = style,
      legend.format = list(digits = dig, text.separator = " - ")
    ) +
    tm_shape(ott_mask) +
    tm_fill(col = 'grey90', alpha = 1) +
    # tm_add_legend(title=lrg.tit)+
    tm_legend(
      legend.text.size = 1.8,
      legend.title.size = 0.001,
      # legend.bg.color = "white",
      legend.position = c(0.5, 0.05)
    ) +
    tm_layout(
      lrg.tit,
      title.size = 3,
      legend.height = 20,
      legend.title.color = NA
    )
}

p1 <- riv_plot("Width_m", "Width (m)")
tmap_save(
  p1,
  'R_Otter_workflow/5_LocationMap/map/widthv1.png',
  height = 18,
  units = 'cm'
)
p2 <- riv_plot(
  "Slope_perc",
  "Slope (%)",
  style = 'quantile',
  col.func = mako(10),
  dig = 2
)
tmap_save(
  p2,
  'R_Otter_workflow/5_LocationMap/map/slopev1.png',
  height = 18,
  units = 'cm'
)
p3 <- riv_plot(
  "BFI_40m",
  'Beaver Forage Index',
  style = "equal",
  dig = 0,
  col.func = rev(mako(10))
)
tmap_save(
  p2,
  'R_Otter_workflow/5_LocationMap/map/BFIv1.png',
  height = 18,
  units = 'cm'
)

me_grid <- gridExtra::grid.arrange(
  grobs = list(tmap_grob(p1), tmap_grob(p2), tmap_grob(p3)),
  # widths = c(3.5, 2),
  layout_matrix = rbind(c(1), c(2), c(3)),
  respect = F
)

ggsave(
  'R_Otter_workflow/5_LocationMap/map/loc_mapV2.png',
  me_grid,
  width = 5,
  height = 14,
  units = 'cm'
)
