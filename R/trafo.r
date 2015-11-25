#' Method to transform values
#' 
#' Does a logarithmic transformation on the values.
#' 
#' @param x values to be transformed
#' @param base logarithmic base
#' @export
trafoLog <- function(x, base=logBase) {
  log(x, base)
}

