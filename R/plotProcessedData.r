#' @rdname plot
#' @aliases plot
#' @export
plotProcessedData <- function(x,
                              xlab = 'Length [m]',
                              ylab = 'Depth [m]',
                              main = paste(x@title, 'without topography'),
                              ...) {
  plot(
    x@processedData@points$dist,
    x@processedData@points$depth,
    xlab = xlab,
    ylab = ylab,
    main = main,
    ...
  )
}

#' @rdname plot
#' @aliases plot
#' @export
plotProcessedDataWithTopo <-
  function(x,
           xlab = 'Length [m]',
           ylab = 'Height [m]',
           main = paste(x@title, 'with topography'),
           ...) {
    plot(
      x@processedData@pointsWithTopo$dist,
      x@processedData@pointsWithTopo$height,
      xlab = xlab,
      ylab = ylab,
      main = main,
      ...
    )
  }