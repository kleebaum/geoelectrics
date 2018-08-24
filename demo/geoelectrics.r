# Script for plotting three geoelectric resistivity measurement profiles of an example measurement

# load filled sinkhole data
data(sinkhole)

plot3dXyz(sinkhole@profiles[[3]])

plot3dXyz(sinkhole,
          xlab = "length [m]",
          ylab = "height above sea level [m]",
          zlab = "length [m]")

plotLegend(sinkhole)
plotLegend(p3)

plotIntersect(sinkhole)
plotIntersect(sinkhole@profiles[[1]], sinkhole@profiles[[2]])

plotRawHeight(p1, height = p1@xyzData@height)
levelplotRaw(p1)
levelplotLegendLabel()

levelplotXyz(p1)