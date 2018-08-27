# Script for plotting three profiles of an example measurement
# filled sinkhole

### looking for working directory
#whereFrom=as.character(sys.calls()[[1]][2])
#try(setwd(dirname(whereFrom)))

### load packages
library(geoelectrics)
# library(lattice) # for levelplots
# library(rgl)
# library(fields)

# a new object is created for each profile
p1 <- new(
  'Profile',
  title = 'Profile 1',
  processedData =
    new('ProcessedData',
        address = './inst/extdata/processed/p1_DipolDipol_SW-NE.xyz'),
  rawData =
    new('RawData',
        address = './inst/extdata/raw/p1_DipolDipol_SW-NE.dat'),
  measurementType = 'DipoleDipole',
  gpsCoordinates =
    new('GpsCoordinates',
        address = './inst/extdata/gps/p1.txt')
)

p2 <- new(
  'Profile',
  title = 'Profile 2',
  processedData =
    new('ProcessedData',
        address = './inst/extdata/processed/p2_DipolDipol_SSW-NNE.xyz'),
  rawData =
    new('RawData',
        address = './inst/extdata/raw/p2_DipolDipol_SSW-NNE.dat'),
  measurementType = 'DipoleDipole',
  gpsCoordinates =
    new('GpsCoordinates',
        address = './inst/extdata/gps/p2.txt')
)

p3 <- new(
  'Profile',
  title = 'Profile 3',
  processedData =
    new('ProcessedData',
        address = './inst/extdata/processed/p3_DipolDipol_S-N.xyz'),
  rawData =
    new('RawData',
        address = './inst/extdata/raw/p3_DipolDipol_S-N.dat'),
  measurementType = 'DipoleDipole',
  gpsCoordinates =
    new('GpsCoordinates',
        address = './inst/extdata/gps/p3.txt')
)

p3 <- adjustHeight(p3, -10)

sinkhole <- new('ProfileSet',
                profiles = list(p1, p2, p3),
                title = 'Sinkhole')

# remove(.Random.seed)
# remove(p1)
# remove(p2)
# remove(p3)
# save.image(file = './data/sinkhole.RData', ascii = TRUE, compress = 'xz')