#' A class to handle a single profile
#'
#' @param number number of the profile
#' @param xyzData object of XyzData class
#' @param rawData object of rawData class
#' @param measurementType type of measurement (e.g. Dipole Dipole, Wenner, ...)
#' @param gpsCoordinates object of GpsCoordinates class
#' @export
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