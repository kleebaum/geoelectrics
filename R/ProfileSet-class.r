#' A class to handle a collection of many profiles
#'
#' @param title title to plot
#' @param profiles list that contains objects of class Profile
#' @slot minLat minimum latitude value of all profiles
#' @slot minLon minimum longitude value of all profiles
#' @slot minData minimum data value of all profiles
#' @slot maxData maximum data value of all profiles
#' @export
setClass("ProfileSet",
         representation = representation(
           profiles = "list",
           title = "character",
           minLat = "numeric",
           minLon = "numeric",
           minData = "numeric",
           maxData = "numeric"))
setMethod("initialize", "ProfileSet",
          function(.Object, profiles=list(), title="",
                   minData=9999999, maxData=0,
                   minLat=100000000000, minLon=100000000000) {
            .Object@profiles <- profiles
            .Object@title <- title         
            
            for (profile in profiles) {
              minDataX <- min(profile@xyzData@seaLevel$val)
              maxDataX <- max(profile@xyzData@seaLevel$val)
              if(minDataX < minData) minData <- minDataX
              if(maxDataX > maxData) maxData <- maxDataX
              
              minLatX <- min(profile@gpsCoordinates@exact$lat)
              minLonX <- min(profile@gpsCoordinates@exact$lon)
              if(minLatX < minLat) minLat <- minLatX
              if(minLonX < minLon) minLon <- minLonX
            }
            
            .Object@minLat <- minLat
            .Object@minLon <- minLon
            .Object@minData <- minData
            .Object@maxData <- maxData
            
            number <- 1
            for(profile in profiles) {
              .Object@profiles[[number]]@number <- number
              
              relativeCoords <- calcRelativeCoords(profile@gpsCoordinates, minLat, minLon)
              .Object@profiles[[number]]@gpsCoordinates@relative <- relativeCoords
              .Object@profiles[[number]]@gpsCoordinates@lmRelative <- lm(relativeCoords$lat ~ relativeCoords$lon)
              
              number <- number + 1
            }
            return(.Object)
          })