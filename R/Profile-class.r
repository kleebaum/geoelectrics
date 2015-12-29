#' Profile Class
#' 
#' A class to handle a single profile.
#'
#' @param number index of the profile.
#' @param xyzData object of Xyz Data Class (\code{\link{XyzData-class}}).
#' @param rawData object of Raw Data Class (\code{\link{RawData-class}}).
#' @param measurementType type of measurement (e.g. Dipole Dipole, Wenner, ...).
#' @param gpsCoordinates object of GpsCoordinates Class (\code{\link{GpsCoordinates-class}}).
#' @export
#' @seealso \code{\link{XyzData-class}}, \code{\link{RawData-class}},
#' \code{\link{GpsCoordinates-class}}, \code{\link{plot3dXyz}}
#' @examples 
#' # p1 <- new("Profile",
#' #           title = "Profile 1",
#' #           xyzData = 
#' #             new("XyzData", 
#' #           rawData = 
#' #             new("RawData",
#' #                 address = "../example/rawdata/p1_DipolDipol_SW-NE.dat"),
#' #           measurementType = "DipolDipol",
#' #           gpsCoordinates = 
#' #             new("GpsCoordinates",
#' #                 address = "../example/gps/p1.txt"))
#' #
#' # p1@title
#' # p1@xyzData
#' # p1@rawData
#' # p1@measurementType
#' # p1@gpsCoordinates
#' #
#' # plot3dXyz(p1)
setClass("Profile",
         representation = representation(
           title = "character",
           number = "numeric",
           xyzData = "XyzData",
           rawData = "RawData", 
           measurementType = "character",
           gpsCoordinates = "GpsCoordinates"),
         prototype = prototype(
           number = 0,
           title = "",
           xyzData = NULL,
           rawData = NULL))