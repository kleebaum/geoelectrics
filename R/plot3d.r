#' 3D Scatterplot of Geoelectrics Profiles
#'
#' Plots the interpolated resistance values of the
#' processed data for a single profile or a set of profiles.
#'
#' @param x either an object of a single Profile or a ProfileSet.
#' @param title title to be plotted.
#' @param sub subtitle to be plotted.
#' @param xlab label of the x-axes, e.g. length [m].
#' @param ylab label of the y-axes, e.g. height above sea level [m].
#' @param zlab label of the z-axes, e.g. length [m].
#' @param minData mimimum value to adjust color bar.
#' @param maxData maximum value to adjust color bar.
#' @param col vector of colors.
#' @param trafo transformation to be done on data (default: log).
#' @param psize size of value points (default: 10).
#' @param ... parameters passed to points3d method of rgl package
#' @rdname plot3d
#' @aliases plot3d
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{ProfileSet-class}},
#' \code{\link{plot}}, \code{\link{levelplot}}
#' @examples
#' data(sinkhole)
#'
#' plot3d(sinkhole@profiles[[1]])
#' plot3d(sinkhole)
setMethod('plot3d', signature(x = 'ProfileSet'),
          function(x,
                   title = x@title,
                   sub = '',
                   xlab = '',
                   ylab = '',
                   zlab = '',
                   minData = x@minData,
                   maxData = x@maxData,
                   col = colors,
                   trafo = log,
                   psize = pointsize,
                   ...) {
            lapply(
              x@profiles,
              plot3d,
              minData = minData,
              maxData = maxData,
              col = col,
              trafo = trafo,
              psize = psize,
              ...
            )
            title3d(title, sub, xlab, ylab, zlab)
          })

#' @rdname plot3d
#' @aliases plot3d
#' @export
setMethod('plot3d', signature(x = 'Profile'),
          function(x,
                   title = '',
                   sub = '',
                   xlab = '',
                   ylab = '',
                   zlab = '',
                   minData = x@processedData@minData,
                   maxData = x@processedData@maxData,
                   col = colors,
                   trafo = log,
                   psize = pointsize,
                   ...) {
            title3d(title, sub, xlab, ylab, zlab)
            values <- trafo(x@processedData@pointsWithTopo$val)
            colorAssignment <-
              myColorRamp(col, values, trafo(minData), trafo(maxData))
            
            l <- x@processedData@pointsWithTopo$dist # hypotenuse
            m <-
              x@gpsCoordinates@lmRelative$coefficients[2] # y = m * dx + n
            n <- x@gpsCoordinates@lmRelative$coefficients[1]
            alpha <- atan(m)
            
            # calculate adjacent leg
            dx <- cos(alpha) * l
            
            # get starting point and adjust
            start.x <- min(x@gpsCoordinates@relative$lon)
            dx <- dx + start.x
            
            # calculate opposite leg
            y <- m * dx + n
            
            # plot 3D
            rgl.bg(color = 'white')
            points3d(
              y,
              x@processedData@pointsWithTopo$height,
              dx,
              color = colorAssignment,
              size = psize,
              ...
            )
            rgl.bbox()
            rgl.texts(
              y[1],
              x@processedData@pointsWithTopo$height[1] + 20,
              dx[1],
              text = paste(x@title),
              cex = 1,
              color = 'black'
            )
            axes3d(edges = 'bbox',
                   yunit = 25,
                   expand = 1.2)
          })