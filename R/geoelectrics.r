# 3D-Visualization of Geoelectric Resistivity Measurement Profiles
# Anja Kleebaum

#' @import lattice
#' @import rgl
#' @import fields
#' @import methods
#' @importFrom grid grid.text
#' @importFrom grDevices colorRamp colorRampPalette rgb
#' @importFrom graphics axis legend plot points
#' @importFrom stats lm
#' @importFrom utils read.table
#' @importFrom utils installed.packages

###---Settings---####
pointsize <- 10
colors <- c("blue", "green", "yellow", "orange", "red", "purple")
logBase <- 10

#' @author Anja Kleebaum
#' @description Electrical resistivity tomography is an efficient geophysical technique to investigate the spatial extent of subsurface structures. Many scientific objectives in geology demand three-dimensional imaging. 
#' 3D electrical resistivity tomography provides a technique to survey three-dimensional structures. 
#' Nonetheless, 3D electrical resistivity tomography requires an enormous amount of time as well as a high work load. 
#' In most cases, 2D electrical resistivity tomography is used to obtain two-dimensional subsurface profiles. 
#' This R package enables the user to visualize two-dimensional profiles in three dimensions.
#' @keywords internal
#' @examples 
#' library(geoelectrics)
#' demo(geoelectrics)
"_PACKAGE"