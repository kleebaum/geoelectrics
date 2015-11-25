#' Maps color to resistivity value
#' 
#' Maps color to resistivity values. 
#'   
#' @param col vector of colors
#' @param values vector of values
#' @param minData minimum value
#' @param maxData maximum value
#' @export
myColorRamp <- function(col, values, minData, maxData) {
  v <- (values - minData) / diff(range(minData, maxData)) # same colors for all profiles
  x <- colorRamp(col)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
} 