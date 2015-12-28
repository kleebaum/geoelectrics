#' Raw Data Class
#' 
#' A class to handle geoelectrics raw data.
#'
#' @param address address of the raw data ascii file.
#' @slot seaLevel data frame that contains raw data positions and resitance values.
#' @slot height data frame that contains topography information (distance and height).
#' @export
#' @examples 
#' rawData = new("RawData",
#'                address = "../example/rawdata/p1_DipolDipol_SW-NE.dat")
#' data(sinkhole)
#' sinkhole@profile[[2]]@rawData
#' sinkhole@profile[[2]]@rawData@address
#' sinkhole@profile[[2]]@rawData@height
#' sinkhole@profile[[2]]@rawData@seaLevel
#' @seealso \code{\link{Profile-class}}, \code{\link{ProfileSet-class}}
setClass("RawData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame",
           height = "data.frame"))
setMethod("initialize", "RawData",
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("Raw Data address is missing.")
            } else {
              .Object@address = address
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
              
              skipLines2 <- skipLines1 + numberOfRows1
              
              try({
                while(!grepl(".", oneLine, fixed=T)) {
                  oneLine <- readLines(con, n=1)
                  skipLines2 <- skipLines2 + 1
                }
                
                while(grepl(".", oneLine, fixed=T)) {
                  oneLine <- readLines(con, n=1)
                  numberOfRows2 <- numberOfRows2 + 1
                }              
                
                height <- read.table(file=address, skip=skipLines2, 
                                     header=F, nrows=numberOfRows2)
                
                .Object@height <- data.frame(
                  "dist"=height[1], 
                  "height"=height[2])
                colnames(.Object@height) <- c("dist", "height")
              })
              
              close(con)              
            }
            return(.Object)
          })