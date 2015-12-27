#' Plot raw points
#' 
#' Plots points of the raw data to show measurement gaps. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @param col color code or name, see \code{\link{par}}.
#' @param pch pplotting ‘character’, i.e., symbol to use. 
#' This can either be a single character or an integer code for one of a set of graphics symbols.
#' @param cex character (or symbol) expansion: a numerical vector. 
#' This works as a multiple of \code{\link{par}}("cex").
#' @export
plotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography"), ...) {  
  plot(Profile@rawData@seaLevel$dist, -1*(Profile@rawData@seaLevel$depth), 
       xlab=xlab, ylab=ylab, main=main, asp=1, ...) 
}

#' Adds topography
#' @export
addTopo <- function(Profile) {
  for(i in 1:nrow(Profile@rawData@height)) {
    dummy <- which(Profile@rawData@height$dist[i] == ceiling(Profile@rawData@seaLevel$dist))
    for(j in 1:length(dummy)) {
      Profile@rawData@heightAdaption$depth[dummy[j]] <- 
        -Profile@rawData@seaLevel$depth[dummy[j]] +
        Profile@rawData@height$height[i] 
    }
  }
  return(Profile)
}

#' Plot raw points after height adaption
#' 
#' Plots points of the raw data to show measurement gaps. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @param col color code or name, see \code{\link{par}}.
#' @param pch pplotting ‘character’, i.e., symbol to use. 
#' This can either be a single character or an integer code for one of a set of graphics symbols.
#' @param cex character (or symbol) expansion: a numerical vector. 
#' This works as a multiple of \code{\link{par}}("cex").
#' @export
plotRawHeight <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                          main=paste(Profile@title, "without topography"), ...) { 
  #Profile <- addTopo(Profile)
  plot(Profile@rawData@heightAdaption$dist, Profile@rawData@heightAdaption$depth, 
       xlab=xlab, ylab=ylab, main=main, asp=1, ...) 
}