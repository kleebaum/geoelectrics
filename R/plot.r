#' Plot Geoelectrics Data Points
#'
#' Plots the geoelectrics data points of a profile.
#' @param x profile object.
#' @param dataType specify whether 'processed' (default) or 'raw' data should be plotted
#' @param withTopo TRUE if topography information is plotted
#' @param xlab label for x-axes.
#' @param ylab label for y-axes.
#' @param main title to be plotted.
#' @param asp the y/x aspect ratio (default: 1).
#' @param ... plot parameters (such as pch, cex, col, ...).
#' @rdname plot
#' @aliases plot
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{plot3d}},
#' \code{\link{levelplot}}
#' @examples
#' data(sinkhole)
#'
#' plot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = FALSE)
#' plotProcessedData(sinkhole@profiles[[1]])
#' 
#' plot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = TRUE)
#' plotProcessedDataWithTopo(sinkhole@profiles[[1]])
#' 
#' plot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = FALSE)
#' plotRawData(sinkhole@profiles[[1]])
#' 
#' plot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = TRUE)
#' plotRawDataWithTopo(sinkhole@profiles[[1]])
setMethod('plot', 'Profile', function(x,
                                      dataType = 'processed',
                                      withTopo = T,
                                      xlab = 'Length [m]',
                                      ylab = 'Height [m]',
                                      main = paste(x@title, 'with topography'),
                                      asp = 1,
                                      ...) {
  if (dataType == 'processed') {
    if (withTopo) {
      plotProcessedDataWithTopo(x, xlab, ylab, main, ..., asp=asp)
    } else {
      plotProcessedData(x, xlab, ylab, main, ..., asp=asp)
    }
  } else {
    if (withTopo) {
      plotRawDataWithTopo(x, xlab, ylab, main, ..., asp=asp)
    } else {
      plotRawData(x, xlab, ylab, main, ..., asp=asp)
    }
  }
})