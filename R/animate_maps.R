
#' Function to Animate a list of maps generated with either `plot_territories` or `plot_forage_density`
#'
#' @export
animate_maps <- function(map_list, filename=NULL, x_pix=1200, y_pix=675, pix_res=96, n_frames=10, fps = 5){

  img <- magick::image_graph(x_pix, y_pix, res = pix_res)
  print(map_list)
  dev.off()
  animation <- magick::image_animate(magick::image_morph(img,  frames = n_frames), fps = fps, optimize = TRUE)

  if (!is.null(filename)){
    image_write(animation, normalizePath(file.path(filename)))
  }

  return(animation)

}
