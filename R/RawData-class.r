
###----Classes and constructors----####
#' A class to handle geoelectrics raw data
#'
#' @param address address of the raw data ascii file
#' @slot seaLevel data frame that contains raw data positions and resitance values 
#' @export
setClass("RawData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame",
           height = "data.frame",
           heightAdaption = "data.frame"))
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
                
                height <- spline(height, xmin=min(profile[1]), xmax=max(profile[1]),
                                 n=(max(profile[1])-min(profile[1])+1))
                
                .Object@height <- data.frame(
                  "dist"=height[[1]], 
                  "height"=height[[2]])
                colnames(.Object@height) <- c("dist", "height")
                
                .Object@heightAdaption <- data.frame(
                  "dist"=.Object@seaLevel$dist, 
                  "depth"=-1*.Object@seaLevel$depth, 
                  "val"=.Object@seaLevel$val)
                colnames(.Object@heightAdaption) <- c("dist", "depth", "val")
                
                for(i in 1:nrow(.Object@height)) {
                  countValues <- which(round(.Object@height[i,1]) == round(.Object@seaLevel$dist))
                  if (length(countValues) > 0)
                    for(j in 1:length(countValues)) {
                      .Object@heightAdaption$depth[countValues[j]] <- .Object@heightAdaption$depth[countValues[j]] + .Object@height[i,2] 
                    }
                }               
                
                #dummy <- which(.Object@height[numberOfRows2,1] < ceiling(.Object@seaLevel$dist))
                #for(j in 1:length(dummy)) {
                #  .Object@heightAdaption$depth[dummy[j]] <- .Object@heightAdaption$depth[dummy[j]] + .Object@height[numberOfRows2,2]
                #}
                
                #.Object@heightAdaption <- .Object@heightAdaption[which(.Object@heightAdaption$depth > 2),]
              })
              
              close(con)              
            }
            return(.Object)
          })