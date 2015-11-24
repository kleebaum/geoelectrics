#' Plots the legend 
#' 
#' Plots the legend for resistivity values.
#' 
#' @param .Object either a single Profile or a ProfileSet
#' @param legend.lab label of legend (default: expression(paste("Resistivity [", Omega, "]")))
#' @param minData minimum value
#' @param maxData maximum value
#' @param lab.breaks number of breaks
#' @param nlevel number of color levels
#' @param col colors
#' @export
setGeneric("plotLegend", function(.Object, 
                                  legend.lab=expression(paste("Resistivity [", Omega, " m]")),
                                  minData=0, maxData=999999,
                                  lab.breaks=c(), nlevel=128,
                                  col=colors) {    
  if(length(lab.breaks) > 0)
    nlevel <- length(lab.breaks)
  standardGeneric("plotLegend")
  image.plot(legend.only=TRUE, add=F,
             zlim= c(minData, maxData),
             legend.lab = legend.lab,
             nlevel=nlevel, 
             col=colorRampPalette(colors)(nlevel-1),
             lab.breaks=lab.breaks, midpoint=T, 
             legend.mar=10, horizontal=T)
  print(maxData)
})

#' @rdname plotLegend
#' @export
setMethod("plotLegend", signature(.Object="ProfileSet"),
          function(.Object, legend.lab,
                   minData=.Object@minData, maxData=.Object@maxData,
                   lab.breaks, nlevel) {
          })

#' @rdname plotLegend
#' @export
setMethod("plotLegend", signature(.Object="Profile"),
          function(.Object, legend.lab,
                   minData=.Object@xyzData@minData, 
                   maxData=.Object@xyzData@maxData,
                   lab.breaks, nlevel) {
          })