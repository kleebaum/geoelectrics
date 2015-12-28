#' Levelplot of Raw Data
#' 
#' Plots raw data values without topography (height adjustment).
#' The raw data values have not been inverted yet.
#' @param Profile profile.
#' @param xlab label for x-axes.
#' @param ylab label for y-axes.
#' @param main title to be plotted.
#' @param trafo function to transform raw data values (default: log).
#' @param col vector of colors.
#' @export
#' @examples 
#' data(sinkhole)
#' levelplotRaw(sinkhole@profiles[[1]])
#' levelplotRaw(sinkhole@profiles[[2]])
#' levelplotRaw(sinkhole@profiles[[3]])
levelplotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                         main=paste(Profile@title, "without topography"), 
                         col=colors, trafo=log, 
                         aspect="iso", interpolate=T, ...) {
  levelplot(round(trafo(Profile@rawData@seaLevel$val)) ~ round(Profile@rawData@seaLevel$dist) * round(-1*Profile@rawData@seaLevel$depth), 
            col.regions = colorRampPalette(col), interpolate=interpolate, 
            regions=T, xlab=xlab, ylab=ylab, main=main, aspect=aspect,
            panel = lattice.getOption("panel.levelplot"), ...)
}