#' Method to calculate relative coordinates 
#' 
#' Calculates relative coordinates in meter from GPS coordinates 
#' (either in UTM or Gauss Krueger).
#' 
#' @param coords exact coordinates of a single Profile
#' @param minLat starting point (latititude)
#' @param minLon starting point (longitude)
#' @export
calcRelativeCoords <- function(coords, minLat, minLon) {
  # latitude and longitude
  if(max(coords@exact$lat) < 180) {
    # grad
    relativeCoords <- data.frame(
      lat=(coords@exact$lat-minLat)*111000,
      lon=(coords@exact$lon-minLon)*72000)
  }
  else {
    # utm
    relativeCoords <- data.frame(
      lat=(coords@exact$lat-minLat),
      lon=(coords@exact$lon-minLon))
  }     
  return(relativeCoords)
}