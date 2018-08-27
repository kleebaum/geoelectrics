#' Gets the Height Information for a Profile
#'
#' Returns the heights for certain distances along the profile (topography information).
#'
#' @param object a single Profile.
#' @return data frame containing distances and heights along the profile
#' @export
#' @seealso \code{\link{GpsCoordinates-class}}, \code{\link{Profile-class}},
#' \code{\link{ProcessedData-class}}
#' @examples
#' data(sinkhole)
#' 
#' getHeightInformation(sinkhole@profiles[[1]]@processedData)
setGeneric('getHeightInformation', function(object) {
  standardGeneric('getHeightInformation')
})

#' @rdname getHeightInformation
#' @export
setMethod('getHeightInformation', 'ProcessedData',
          function(object) {
            heightInformation <- data.frame(dist = double(),
                                            height = double())
            
            j <- 1
            for (i in 1:max(object@pointsWithTopo[1])) {
              indices <- which(round(object@pointsWithTopo[1]) == i)
              if (length(indices) > 0) {
                index <- min(indices)
                heightInformation[j, ] <-
                  c(object@pointsWithTopo[index, 1], object@pointsWithTopo[index, 2])
                j <- j + 1
              }
            }
            heightInformation
          })