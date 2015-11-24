#' Plot raw points
#' 
#' Plots points of the raw data to show measurement gaps. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {  
  plot(Profile@rawData@seaLevel$dist, -1*(Profile@rawData@seaLevel$depth), 
       xlab=xlab, ylab=ylab, main=main, asp=1) 
}