#' A class to handle a single profile
#'
#' @param number number of the profile
#' @param xyzData object of XyzData class
#' @param rawData object of rawData class
#' @param measurementType type of measurement (e.g. Dipole Dipole, Wenner, ...)
#' @param gpsCoordinates object of GpsCoordinates class
#' @export
#' @examples
#' new("Profile", 
#'      number = 1,
#'      xyzData = 
#'        new("XyzData", 
#'             address = "../example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
#'      rawData = 
#'        new("RawData",
#'             address ="../example/rawdata/p1_DipolDipol_SW-NE.dat"),
#'      measurementType = "DipolDipol",
#'      gpsCoordinates = 
#'        new("GpsCoordinates",
#'             address ="../example/gps/p1.txt"))
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