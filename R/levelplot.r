#' Levelplot of Geoelectrics Data
#'
#' Plots the interpolated resistance values of the
#' geoelectrics data.
#' @param x profile object.
#' @param data is always NULL
#' @param dataType specify whether 'processed' (default) or 'raw' data should be plotted
#' @param withTopo TRUE if topography information is plotted
#' @param xlab label for x-axes.
#' @param ylab label for y-axes.
#' @param main title to be plotted.
#' @param col vector of colors.
#' @param breaks number of color breaks.
#' @param trafo transformation to be done on data (default: log).
#' @param backtrafo back transformation to plot correct labels (default: exp).
#' @param ... lattice levelplot arguments.
#' @rdname levelplot
#' @aliases levelplot
#' @export
#' @seealso \code{\link{Profile-class}}
#' @examples
#' data(sinkhole)
#'
#' levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = FALSE)
#' levelplotLegendLabel()
#'
#' levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = TRUE)
#' levelplotLegendLabel()
#'
#' levelplot(sinkhole@profiles[[1]], dataType = 'raw')
#' levelplotLegendLabel()
setGeneric('levelplot')

#' @rdname levelplot
#' @aliases levelplot
#' @export
setMethod('levelplot', signature('Profile', 'ANY'),
          function(x,
                   dataType = 'processed',
                   withTopo = FALSE,
                   xlab = 'Length [m]',
                   ylab = 'Depth [m]',
                   main = paste(x@title),
                   col = colors,
                   breaks = 18,
                   trafo = log,
                   backtrafo = exp,
                   ...) {
            if (dataType == 'processed') {
              if (withTopo) {
                levelplotProcessedDataWithTopo(x, xlab, ylab, main, col, breaks, trafo, backtrafo, ...)
              } else {
                levelplotProcessedData(x, xlab, ylab, main, col, breaks, trafo, backtrafo, ...)
              }
            } else {
              levelplotRawData(x, xlab, ylab, main, col, trafo, ...)
            }
          })

#' @rdname levelplot
#' @aliases levelplot
#' @export
setMethod('levelplot', signature(x = 'ProfileSet'),
          function(x,
                   dataType = 'processed',
                   withTopo = FALSE,
                   xlab = 'Length [m]',
                   ylab = 'Depth [m]',
                   main = paste(x@title),
                   col = colors,
                   breaks = 18,
                   trafo = log,
                   backtrafo = exp,
                   ...) {
            lapply(
              x@profiles,
              levelplot,
              dataType = dataType,
              withTopo = withTopo,
              xlab = xlab,
              ylab = ylab,
              main = main,
              col = col,
              breaks = breaks,
              trafo = trafo,
              backtrafo = backtrafo,
              ...
            )
          })

#' Levelplot Legend Label
#'
#' Plots the label of the levelplot.
#' @param legend.lab label (default: 'Resistivity').
#' @param unit unit (default: 'Ohm*m').
#' @export
#' @seealso \code{\link{levelplot}}
#' @examples
#' data(sinkhole)
#'
#' levelplot(sinkhole@profiles[[1]])
#' levelplotLegendLabel()
#'
#' levelplot(sinkhole@profiles[[2]])
#' levelplotLegendLabel()
#'
#' levelplot(sinkhole@profiles[[3]])
#' levelplotLegendLabel()
levelplotLegendLabel <- function(legend.lab = 'Resistivity',
                                 unit = expression(paste('[', Omega, 'm]'))) {
  trellis.focus('legend',
                side = 'right',
                clipp.off = TRUE,
                highlight = FALSE)
  grid.text(legend.lab, 0.2, 0, hjust = 0.5, vjust = 1.5)
  grid.text(unit, 0.2, 0, hjust = 0.5, vjust = 2.5)
  trellis.unfocus()
}