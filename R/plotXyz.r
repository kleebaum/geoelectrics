#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data. 
#' @param Profile profile
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotXyz <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {
  plot(Profile@xyzData@seaLevel$dist, Profile@xyzData@seaLevel$depth, 
       xlab=xlab, ylab=ylab, main=main, asp=1)
}

#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data
#' after height adjustment. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotXyzHeight <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                          main=paste(Profile@title, "with topography")) {
  plot(Profile@xyzData@heightAdaption$dist, Profile@xyzData@heightAdaption$depth, 
       xlab=xlab, ylab=ylab, main=main, asp=1)
}