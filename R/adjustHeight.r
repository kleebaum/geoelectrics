#' Adjust Profile Height
#'
#' Adjusts the height of a single profile (adds a delta value to ALL data points).
#' This is necessary if GPS measurement heights of two profiles differ systematically.
#'
#' @param object a single Profile.
#' @param delta positive or negative value.
#' @return adjusted profile
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
#' p3 <- adjustHeight(p3, -10)
setGeneric('adjustHeight', function(object, delta) {
  standardGeneric('adjustHeight')
})

#' @rdname adjustHeight
#' @export
setMethod('adjustHeight', 'Profile',
          function(object, delta) {
            object@xyzData@heightAdaption$depth <-
              object@xyzData@heightAdaption$depth + delta
            object
          })