#' Adjust Profile Height
#'
#' Adjusts the height of a single profile.
#' GPS measurement heights might differ otherwise.
#'
#' @param Profile a single Profile.
#' @param deltaMeter positive or negative value.
#' @export
#' @seealso \code{\link{GpsCoordinates-class}}, \code{\link{Profile-class}}
#' @examples
#' p3 <- new(
#'   "Profile",
#'   title = "Profile 3",
#'   xyzData =
#'     new("XyzData",
#'         address = system.file("extdata/processed/p3_DipolDipol_S-N.xyz", 
#'                   package='geoelectrics')),
#'   rawData =
#'     new("RawData",
#'         address = system.file("extdata/raw/p3_DipolDipol_S-N.dat", 
#'                   package='geoelectrics')),
#'   measurementType = "DipolDipol",
#'   gpsCoordinates =
#'     new("GpsCoordinates",
#'         address = system.file("extdata/gps/p3.txt", 
#'                   package='geoelectrics'))
#' )
#'
#' p3 <- heightAdjustment(p3, -10)
heightAdjustment <- function(Profile, deltaMeter) {
  Profile@xyzData@heightAdaption$depth <-
    Profile@xyzData@heightAdaption$depth + deltaMeter
  return(Profile)
}