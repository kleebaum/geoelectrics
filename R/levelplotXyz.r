#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the xyz 
#' data without height adjustment.
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
levelplotXyz <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                         main=paste(Profile@title, "without topography")) {
  levelplot(trafo(Profile@xyzData@seaLevel$val) ~ Profile@xyzData@seaLevel$dist * Profile@xyzData@seaLevel$depth, 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab=xlab, ylab=ylab, main=main)
}

#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the 
#' xyz data after height adjustment.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @param Profile profile.
#' @export
levelplotXyzHeight <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                               main=paste(Profile@title, "with topography")) {
  levelplot(trafo(Profile@xyzData@heightAdaption$val) ~ round(Profile@xyzData@heightAdaption$dist) * round(Profile@xyzData@heightAdaption$depth), 
            col.regions = colorRampPalette(colors), interpolate=F, 
            regions=T, xlab=xlab, ylab=ylab, main=main)
}
