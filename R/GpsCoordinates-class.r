#' GPS Coordinates Class
#'
#' A class to handle gps coordinates.
#'
#' @param address address of the gps ascii file
#' @slot exact data frame that contains measured gps coordinates
#' @slot lm linear model of the measured gps coordinates
#' @slot relative relative coordinates
#' @slot lmRelative linear model of relative coordinates  
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{ProfileSet-class}},
#' \code{\link{heightAdjustment}}, \code{\link{calcRelativeCoords}}
#' @examples 
#' # gpsCoordinates = new("GpsCoordinates",
#' #                      address = "../example/gps/p1.txt")
#' data(sinkhole)
#' sinkhole@profiles[[1]]@gpsCoordinates
#' sinkhole@profiles[[1]]@gpsCoordinates@address
#' sinkhole@profiles[[1]]@gpsCoordinates@exact
#' sinkhole@profiles[[1]]@gpsCoordinates@lm
#' sinkhole@profiles[[1]]@gpsCoordinates@relative
#' sinkhole@profiles[[1]]@gpsCoordinates@lmRelative
setClass("GpsCoordinates",
         representation = representation(
           address = "character",
           exact = "data.frame",
           lm = "lm",
           relative = "data.frame",
           lmRelative = "lm"),
         prototype = prototype(
           lm = lm(1~1),
           lmRelative = lm (1~1)))
setMethod("initialize", "GpsCoordinates", 
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("GPS coordinates address is missing.")
            } else {
              .Object@address = address
              
              gpsData <- read.table(file=address, header=T)  
                
              .Object@exact <- data.frame(
                "lat"=gpsData[1],
                "lon"=gpsData[2])
              colnames(.Object@exact) <- c("lat", "lon")
              
              lm <- lm(.Object@exact$lat ~ .Object@exact$lon)
              .Object@lm <- lm
              
              minLat <- min(gpsData[1])
              minLon <- min(gpsData[2])
              
              relativeCoords <- calcRelativeCoords(.Object, minLat, minLon)
              .Object@relative <- relativeCoords
              .Object@lmRelative <- lm(relativeCoords$lat ~ relativeCoords$lon)
            }
            return(.Object)
          })