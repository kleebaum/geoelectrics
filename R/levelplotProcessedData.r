#' @rdname levelplot
#' @aliases levelplot
#' @export
levelplotProcessedData <-
  function(x,
           xlab = 'Length [m]',
           ylab = 'Depth [m]',
           main = paste(x@title, 'without topography'),
           col = colors,
           breaks = 18,
           trafo = log,
           backtrafo = exp,
           aspect = "iso",
           ...) {
    lab.breaks <-
      round(backtrafo(seq(trafo(
        min(x@processedData@pointsWithTopo$val)
      ),
      trafo(
        max(x@processedData@pointsWithTopo$val)
      ),
      length.out = breaks)))
    levelplot(
      trafo(x@processedData@points$val) ~ x@processedData@points$dist * x@processedData@points$depth,
      col.regions = colorRampPalette(col),
      xlab = xlab,
      ylab = ylab,
      main = main,
      aspect = aspect,
      colorkey = list(at = as.numeric(factor(c(
        seq(
          from = 0,
          to = 1,
          length.out = breaks
        )
      ))),
      labels = as.character(lab.breaks)),
      ...
    )
  }

#' @rdname levelplot
#' @aliases levelplot
#' @export
levelplotProcessedDataWithTopo <-
  function(x,
           xlab = 'Length [m]',
           ylab = 'Height [m]',
           main = paste(x@title, 'with topography'),
           col = colors,
           breaks = 18,
           trafo = log,
           backtrafo = exp,
           aspect = "iso",
           ...) {
    lab.breaks <-
      round(backtrafo(seq(trafo(
        min(x@processedData@pointsWithTopo$val)
      ),
      trafo(
        max(x@processedData@pointsWithTopo$val)
      ),
      length.out = breaks)))
    levelplot(
      trafo(x@processedData@pointsWithTopo$val) ~ round(x@processedData@pointsWithTopo$dist) * round(x@processedData@pointsWithTopo$height),
      col.regions = colorRampPalette(col),
      xlab = xlab,
      ylab = ylab,
      main = main,
      aspect = aspect,
      colorkey = list(at = as.numeric(factor(c(
        seq(
          from = 0,
          to = 1,
          length.out = breaks
        )
      ))),
      labels = as.character(lab.breaks)),
      ...
    )
  }