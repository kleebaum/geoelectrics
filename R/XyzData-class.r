#' XYZ Data Class
#' 
#' A class to handle xyz data. 
#' The software Res2DInv produces .xyz-files that contain the
#' inverted resistance values. The xyz class parses .xyz files.
#'
#' @param address address of the xyz ascii file
#' @slot seaLevel data frame that contains positions and values withouth height adjustment
#' @slot heightAdaption data frame that contains positions and values after height adjustment 
#' @slot height data frame that contains topography information (distances and heights). 
#' It is reconstructed from .xyz-file.
#' @slot minData minimum value
#' @slot maxData maximum value
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{ProfileSet-class}}, 
#' \code{\link{plotXyz}}, \code{\link{plotXyzHeight}}, \code{\link{plot3dXyz}}
#' @examples 
#' # xyzData = new("XyzData", 
#' #                address = "../example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
#' 
#' data(sinkhole) 
#' sinkhole@profiles[[1]]@xyzData
#' sinkhole@profiles[[1]]@xyzData@seaLevel
#' sinkhole@profiles[[1]]@xyzData@heightAdaption
#' sinkhole@profiles[[1]]@xyzData@height
#' sinkhole@profiles[[1]]@xyzData@minData
#' sinkhole@profiles[[1]]@xyzData@maxData
setClass("XyzData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame",
           heightAdaption = "data.frame",
           minData = "numeric",
           maxData = "numeric",
           height = "data.frame"))
setMethod("initialize", "XyzData",
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("XYZ data address is missing.")
            } else {
              .Object@address = address
              con  <- file(address, open = "r")
              
              skipLines1 <- 0
              numberOfRows <- 0
              numberOfRows2 <- 0
              
              oneLine <- readLines(con, n=1)
              while(grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                skipLines1 <- skipLines1 + 1
              }
              
              while(!grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                numberOfRows <- numberOfRows + 1    
              }
              
              skipLines2 <- skipLines1 + numberOfRows
              
              while(grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                skipLines2 <- skipLines2 + 1
              }
              
              while(!grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                numberOfRows2 <- numberOfRows2 + 1    
              }
              
              profile_without_topo <- read.table(file=address, skip=skipLines1, 
                                                 header=F, nrows=numberOfRows)
              .Object@seaLevel <- data.frame(
                dist=profile_without_topo[1],
                depth=profile_without_topo[2],
                val=profile_without_topo[3])
              colnames(.Object@seaLevel) <- c("dist", "depth", "val")
              
              profile <- read.table(file=address, skip=skipLines2, 
                                    header=F, nrows=numberOfRows2)
              
              .Object@heightAdaption <- data.frame(
                dist=profile[1],
                depth=profile[2],
                val=profile[3])
              colnames(.Object@heightAdaption) <- c("dist", "depth", "val")
              
              .Object@minData <- min(profile[3])
              .Object@maxData <- max(profile[3])
              
              height <- data.frame(
                dist=1,
                height=1)
            
              j <- 1
              for (i in 1:max(profile[1])) {
                indices <- which(round(profile[1])==i)
                if (length(indices)>0) {
                  index <- min(indices)
                  height[j,] <- c(profile[index,1], profile[index,2])
                  j <- j + 1
                }
              }
              
              .Object@height <- height
              
              close(con)
            }
            return(.Object)               
          })