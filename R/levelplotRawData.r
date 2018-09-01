#' @rdname levelplot
#' @aliases levelplot
#' @export
levelplotRawData <-
  function(x,
           xlab = 'Length [m]',
           ylab = 'Depth [m]',
           main = paste(x@title, 'without topography (raw data)'),
           col = colors,
           trafo = log,
           aspect = "iso",
           ...) {
    levelplot(
      trafo(x@rawData@points$val) ~ x@rawData@points$dist * (-1 *
                                                               x@rawData@points$depth),
      col.regions = colorRampPalette(col),
      xlab = xlab,
      ylab = ylab,
      main = main,
      aspect = aspect,
      ...
    )
  }