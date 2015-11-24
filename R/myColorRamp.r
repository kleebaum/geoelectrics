#' Maps color to resistivity value
#' 
#' Maps color to resistivity values. 
#'   
#' @param colors vector of colors
#' @param values vector of values
#' @param minData minimum value
#' @param maxData maximum value
myColorRamp <- function(colors, values, minData, maxData) {
  v <- (values - minData) / diff(range(minData, maxData)) # same colors for all profiles
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
} 