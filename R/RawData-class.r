
###----Classes and constructors----####
#' A class to handle geoelectrics raw data
#'
#' @param address address of the raw data ascii file
#' @slot seaLevel data frame that contains raw data positions and resitance values 
#' @export
#' @examples 
#' rawDataObject <- new("RawData", address="../example/rawdata/p1_DipolDipol_SW-NE.dat")
setClass("RawData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame"))
setMethod("initialize", "RawData",
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("Raw Data address is missing.")
            } else {
              con  <- file(address, open = "r")
              
              skipLines1 <- 9
              skipLines2 <- 0
              numberOfRows1 <- 0
              numberOfRows2 <- 0
              
              for(i in 1:10) {
                oneLine <- readLines(con, n=1)
              }
              
              while(grepl(".", oneLine, fixed=T)) {
                oneLine <- readLines(con, n=1)
                numberOfRows1 <- numberOfRows1 + 1
              }
              
              profile <- read.table(file=address, skip=skipLines1, 
                                    header=F, nrows=numberOfRows1)
              
              .Object@seaLevel <- data.frame(
                "dist"=profile[1],
                "depth"=profile[2],
                "val"=profile[4])
              colnames(.Object@seaLevel) <- c("dist", "depth", "val")
              
              close(con)              
            }
            return(.Object)
          })