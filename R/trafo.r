#' Transform Values
#' 
#' Transforms values. Does a logarithmic transformation on the values.
#' Can be overwritten.
#' 
#' @param x values to be transformed
#' @param base logarithmic base
#' @export
trafo <- function(x, base=logBase) {
  log(x, base)
}

