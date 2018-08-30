#' Graphical User Interface of the Geoelectrics R Package
#' 
#' This method starts a simple TclTk graphical user interface (GUI) provided by this package.
#' @export
#' @examples
#' geoelectricsGui()
geoelectricsGui <- function() {
  if(!is.element('tkrplot', installed.packages()[,1])) {
    stop('Please install the `tkrplot` package first.')
  }
  source(system.file('gui/gui.r', package = 'geoelectrics'))
}