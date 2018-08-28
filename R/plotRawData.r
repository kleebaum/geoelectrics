#' @rdname plot
#' @aliases plot
#' @export
plotRawData <- function(x,
                    xlab = 'Length [m]',
                    ylab = 'Depth [m]',
                    main = paste(x@title, 'without topography'),
                    ...) {
  plot(
    x@rawData@points$dist,
    -1 * (x@rawData@points$depth),
    xlab = xlab,
    ylab = ylab,
    main = main,
    ...
  )
}

#' @param height topo data frame of distances and height.
#' @param spline if TRUE spline interpolation is conducted.
#' @rdname plot
#' @aliases plot
#' @export
plotRawDataWithTopo <- function(x,
                          xlab = 'Length [m]',
                          ylab = 'Depth [m]',
                          main = paste(x@title, 'with topography'),
                          height = x@processedData@height,
                          spline = TRUE,
                          ...) {
  if (spline) {
    height <- as.data.frame(spline(
      height,
      method = 'natural',
      xmin = min(x@rawData@points$dist),
      xmax = max(x@rawData@points$dist),
      n = (
        max(x@rawData@points$dist) - min(x@rawData@points$dist) +
          1
      )
    ))
  }
  
  heightAdaption <- data.frame(
    'dist' = x@rawData@points$dist,
    'depth' = -1 * x@rawData@points$depth,
    'val' = x@rawData@points$val
  )
  
  for (i in 1:nrow(height)) {
    indices <-
      which(round(height[i, 1]) == round(x@rawData@points$dist))
    if (length(indices) > 0)
      for (j in 1:length(indices)) {
        heightAdaption$depth[indices[j]] <-
          heightAdaption$depth[indices[j]] + height[i, 2]
      }
  }
  plot(
    heightAdaption$dist,
    heightAdaption$depth,
    xlab = xlab,
    ylab = ylab,
    main = main,
    ...
  )
}