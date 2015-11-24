#' Plot raw data levels
#' 
#' Plots the interpolated resistance values of the
#' raw data without height adjustment.
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
levelplotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                         main=paste(Profile@title, "without topography")) {
  levelplot(round(trafo(Profile@rawData@seaLevel$val)) ~ round(Profile@rawData@seaLevel$dist) * round(-1*Profile@rawData@seaLevel$depth), 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab=xlab, ylab=ylab, main=main, aspect="iso",
            panel = lattice.getOption("panel.levelplot"))
}