#' Raw Data Class
#'
#' A class to handle geoelectrics raw data.
#' The raw data class parses .dat files provided by the GeoTest software by Dr. Rauen.
#' If you want to use another format, overwrite the \code{\link{parseRawDataFile}} method.
#'
#' @slot address address of the raw data ascii file.
#' @slot points data frame that contains raw data resistance values and their positions (distance and depth).
#' @export
#' @examples
#' rawData = new('RawData', address = system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
#'           package='geoelectrics'))
#'
#' data(sinkhole)
#' sinkhole@profiles[[2]]@rawData
#' sinkhole@profiles[[2]]@rawData@address
#' sinkhole@profiles[[2]]@rawData@points
#' @seealso \code{\link{parseRawDataFile}}, \code{\link{Profile-class}}, \code{\link{ProfileSet-class}}
setClass(
  'RawData',
  representation = representation(address = 'character',
                                  points = 'data.frame'),
  prototype = prototype(points = data.frame(
    dist = double(),
    depth = double(),
    val = double()
  ))
)
setMethod('initialize', 'RawData',
          function(.Object, address, skip = 9) {
            if (missing(address)) {
              cat('Created an empty raw data object.\n')
            }
            else if (!file.exists(address)) {
              stop('Raw data file address is given but file cannot be found.')
            } else {
              .Object@address = address
              .Object@points <- parseRawDataFile(address, skip)
            }
            return(.Object)
          })

#' Processed Data Class
#'
#' A class to handle processed geoelectrics data in ascii format.
#' The processed data class parses .xyz files produced by the software Res2DInv.
#' If you want to use another format, overwrite the \code{\link{parseProcessedDataFile}} method.
#'
#' @slot address address of the processed ascii file
#' @slot points data frame that contains positions and values withouth topography information
#' @slot pointsWithTopo data frame that contains positions and values with topography information
#' @slot height data frame that contains topography information (distances and heights).
#' It is reconstructed from .xyz-file.
#' @slot minData minimum value
#' @slot maxData maximum value
#' @export
#' @seealso \code{\link{parseProcessedDataFile}}, \code{\link{Profile-class}}, \code{\link{ProfileSet-class}}
#' @examples
#' processedData = new('ProcessedData',
#'                      address = system.file('extdata/processed/p1_DipolDipol_SW-NE.xyz',
#'                      package='geoelectrics'))
#'
#' data(sinkhole)
#' sinkhole@profiles[[1]]@processedData
#' sinkhole@profiles[[1]]@processedData@points
#' sinkhole@profiles[[1]]@processedData@pointsWithTopo
#' sinkhole@profiles[[1]]@processedData@height
#' sinkhole@profiles[[1]]@processedData@minData
#' sinkhole@profiles[[1]]@processedData@maxData
setClass(
  'ProcessedData',
  representation = representation(
    address = 'character',
    points = 'data.frame',
    pointsWithTopo = 'data.frame',
    minData = 'numeric',
    maxData = 'numeric',
    height = 'data.frame'
  ),
  prototype = prototype(
    points =
      data.frame(
        dist = double(),
        depth = double(),
        val = double()
      ),
    pointsWithTopo =
      data.frame(
        dist = double(),
        height = double(),
        val = double()
      )
  )
)
setMethod('initialize', 'ProcessedData',
          function(.Object, address, skip = 0) {
            if (missing(address)) {
              cat('Created an empty processed data object.\n')
            }
            else if (!file.exists(address)) {
              stop('Processed data file address is given but file cannot be found.')
            } else {
              .Object@address = address
              
              parsedProcessedData <-
                parseProcessedDataFile(address, skip)
              .Object@points <- parsedProcessedData[[1]]
              .Object@pointsWithTopo <- parsedProcessedData[[2]]
              
              .Object@minData <- min(.Object@pointsWithTopo[3])
              .Object@maxData <- max(.Object@pointsWithTopo[3])
              
              .Object@height <- getHeightInformation(.Object)
            }
            return(.Object)
          })

#' GPS Coordinates Class
#'
#' A class to handle gps coordinates.
#'
#' @slot address address of the gps ascii file
#' @slot exact data frame that contains measured gps coordinates
#' @slot relative relative coordinates, normalized to (0,0)
#' @slot lm linear model of the measured gps coordinates
#' @slot lmRelative linear model of relative coordinates
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{ProfileSet-class}},
#' \code{\link{adjustHeight}}, \code{\link{calcRelativeCoords}}
#' @examples
#' gpsCoordinates = new('GpsCoordinates', address = system.file('extdata/gps/p1.txt',
#'                  package='geoelectrics'))
#'
#' data(sinkhole)
#' sinkhole@profiles[[1]]@gpsCoordinates
#' sinkhole@profiles[[1]]@gpsCoordinates@address
#' sinkhole@profiles[[1]]@gpsCoordinates@exact
#' sinkhole@profiles[[1]]@gpsCoordinates@lm
#' sinkhole@profiles[[1]]@gpsCoordinates@relative
#' sinkhole@profiles[[1]]@gpsCoordinates@lmRelative
setClass(
  'GpsCoordinates',
  representation = representation(
    address = 'character',
    exact = 'data.frame',
    lm = 'lm',
    relative = 'data.frame',
    lmRelative = 'lm'
  ),
  prototype = prototype(
    exact = data.frame(lat = double(),
                       lon = double()),
    lm = lm(1 ~ 1),
    lmRelative = lm(1 ~ 1)
  )
)
setMethod('initialize', 'GpsCoordinates',
          function(.Object, address) {
            if (missing(address)) {
              cat('Created an empty GPS coordinates object.\n')
            }
            else if (!file.exists(address)) {
              stop('GPS coordinates file address is given but file cannot be found.')
            } else {
              .Object@address = address
              
              gpsData <- read.table(file = address, header = T)
              
              .Object@exact <- data.frame(lat = gpsData[1],
                                          lon = gpsData[2])
              
              .Object@lm <- lm(.Object@exact$lat ~ .Object@exact$lon)

              minLat <- min(gpsData[1])
              minLon <- min(gpsData[2])
              
              relativeCoords <-
                calcRelativeCoords(.Object, minLat, minLon)
              .Object@relative <- relativeCoords
              .Object@lmRelative <-
                lm(relativeCoords$lat ~ relativeCoords$lon)
            }
            return(.Object)
          })

#' Profile Class
#'
#' A class to handle a single profile.
#'
#' @slot title title of the profile (e.g. Profile 1).
#' @slot number index of the profile.
#' @slot processedData object of Processed Data Class (\code{\link{ProcessedData-class}}).
#' @slot rawData object of Raw Data Class (\code{\link{RawData-class}}).
#' @slot measurementType type of measurement (e.g. Dipole Dipole, Wenner, ...).
#' @slot gpsCoordinates object of GpsCoordinates Class (\code{\link{GpsCoordinates-class}}).
#' @export
#' @seealso \code{\link{ProcessedData-class}}, \code{\link{RawData-class}},
#' \code{\link{GpsCoordinates-class}}, \code{\link{plot3d}}, \code{\link{plot}}
#' @examples
#' p1 <- new('Profile',
#'            title = 'Profile 1',
#'            processedData =
#'              new('ProcessedData', address = system.file('extdata/processed/p1_DipolDipol_SW-NE.xyz',
#'                                       package='geoelectrics')),
#'            rawData =
#'              new('RawData', address = system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
#'                                       package='geoelectrics')),
#'            measurementType = 'DipoleDipole',
#'            gpsCoordinates =
#'              new('GpsCoordinates', address = system.file('extdata/gps/p1.txt',
#'                                              package='geoelectrics')))
#'
#' p1@title
#' p1@processedData
#' p1@rawData
#' p1@measurementType
#' p1@gpsCoordinates
#'
#' plot3d(p1)
setClass(
  'Profile',
  representation = representation(
    title = 'character',
    number = 'numeric',
    processedData = 'ProcessedData',
    rawData = 'RawData',
    measurementType = 'character',
    gpsCoordinates = 'GpsCoordinates'
  ),
  prototype = prototype(
    number = 0,
    title = '',
    measurementType = '',
    processedData = new('ProcessedData'),
    rawData = new('RawData'),
    gpsCoordinates = new('GpsCoordinates')
  )
)

#' Profile Set Class
#'
#' A class to handle a collection of many profiles.
#'
#' @slot title title to plot
#' @slot profiles list that contains objects of class Profile (\code{\link{Profile-class}})
#' @slot minLat minimum latitude value of all profiles
#' @slot minLon minimum longitude value of all profiles
#' @slot minData minimum data value of all profiles
#' @slot maxData maximum data value of all profiles
#' @export
#' @seealso \code{\link{Profile-class}}, \code{\link{plot3d}}
#' @examples
#' # sinkhole <- new('ProfileSet',
#' #                profiles = list(p1, p2, p3),
#' #                title='Sinkhole')
#'
#' data(sinkhole)
#' plot3d(sinkhole)
setClass(
  'ProfileSet',
  representation = representation(
    profiles = 'list',
    title = 'character',
    minLat = 'numeric',
    minLon = 'numeric',
    minData = 'numeric',
    maxData = 'numeric'
  )
)
setMethod('initialize', 'ProfileSet',
          function(.Object,
                   profiles = list(),
                   title = '',
                   minData = 9999999,
                   maxData = 0,
                   minLat = 100000000000,
                   minLon = 100000000000) {
            .Object@profiles <- profiles
            .Object@title <- title
            
            for (profile in profiles) {
              minDataX <- min(profile@processedData@points$val)
              maxDataX <- max(profile@processedData@points$val)
              if (minDataX < minData)
                minData <- minDataX
              if (maxDataX > maxData)
                maxData <- maxDataX
              
              minLatX <- min(profile@gpsCoordinates@exact$lat)
              minLonX <- min(profile@gpsCoordinates@exact$lon)
              if (minLatX < minLat)
                minLat <- minLatX
              if (minLonX < minLon)
                minLon <- minLonX
            }
            
            .Object@minLat <- minLat
            .Object@minLon <- minLon
            .Object@minData <- minData
            .Object@maxData <- maxData
            
            number <- 1
            for (profile in profiles) {
              .Object@profiles[[number]]@number <- number
              
              relativeCoords <-
                calcRelativeCoords(profile@gpsCoordinates, minLat, minLon)
              .Object@profiles[[number]]@gpsCoordinates@relative <-
                relativeCoords
              .Object@profiles[[number]]@gpsCoordinates@lmRelative <-
                lm(relativeCoords$lat ~ relativeCoords$lon)
              
              number <- number + 1
            }
            return(.Object)
          })