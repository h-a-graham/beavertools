#Function to add general summary labels for facet plots - code adpated from here:
#  https://stackoverflow.com/questions/36941197/overall-label-for-facets
add_general_facet_labs <- function(p, labelR, labelT){

  # Get the ggplot grob
  z <- ggplotGrob(p)

  # Get the positions of the strips in the gtable: t = top, l = left, ...
  posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
  posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

  # Add a new column to the right of current right strips,
  # and a new row on top of current top strips
  width <- z$widths[max(posR$r)]    # width of current right strips
  height <- z$heights[min(posT$t)]  # height of current top strips

  z <- gtable_add_cols(z, width, max(posR$r))
  z <- gtable_add_rows(z, height, min(posT$t)-1)

  # Construct the new strip grobs
  stripR <- gTree(name = "Strip_right", children = gList(
    rectGrob(gp = gpar(col = 'black', fill = "grey85")),
    textGrob(labelR, rot = -90, gp = gpar(fontsize = 10, col = "grey10")))) # previously 8.8

  stripT <- gTree(name = "Strip_top", children = gList(
    rectGrob(gp = gpar(col = 'black', fill = "grey85")),
    textGrob(labelT, gp = gpar(fontsize = 10, col = "grey10")))) # previously 8.8

  # Position the grobs in the gtable
  z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
  z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

  # Add small gaps between strips
  z <- gtable_add_cols(z, unit(1/5, "line"), max(posR$r))
  z <- gtable_add_rows(z, unit(1/5, "line"), min(posT$t))

  # Draw it
  grid.newpage()
  grid.draw(z)

  return(z)
}
