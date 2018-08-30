\name{sinkhole}
\docType{data}
\alias{sinkhole}
\title{Filled Sinkhole}
\description{Geoelectrics profiles measured at a filled sinkhole.
This data set contains an object of the ProfileSet class.
}
\examples{
data(sinkhole)

# Plot the processed data in three dimensions
plot3d(sinkhole)

# Plot legend for the profile set
plot.new()
plotLegend(sinkhole)

# Plot raw data of profile 1
plot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = FALSE, 
     main = 'Profile 1', ylab = 'Depth [m]')

plot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = TRUE)

levelplot(sinkhole@profiles[[1]], dataType = 'raw')
levelplotLegendLabel()

# Plot processed data of profile 1
plot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = FALSE,
     main = 'Profile 1', ylab = 'Depth [m]')

plot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = TRUE)

levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = FALSE)
levelplotLegendLabel()

levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = TRUE)
levelplotLegendLabel()

# Compare processed data values on the intersection line between two profiles
plotIntersect(sinkhole)
}
\seealso{
\code{\link{ProfileSet-class}}, \code{\link{Profile-class}}, \code{\link{plot3d}}, \code{\link{plot}},
\code{\link{levelplot}}, \code{\link{plotLegend}}, \code{\link{plotIntersect}}
}
\format{Object of Profile Set class including three Profiles.}
\keyword{datasets}