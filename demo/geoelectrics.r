# Script for plotting three geoelectric resistivity measurement profiles of an example measurement

# Create three profiles from example files of a filled sinkhole
readline(prompt = 'Hit <Return> to create the first profile from example files of a filled sinkhole:')

p1 <-
  new(
    'Profile',
    title = 'Profile 1',
    processedData = new(
      'ProcessedData',
      address = system.file('extdata/processed/p1_DipolDipol_SW-NE.xyz',
                            package = 'geoelectrics')
    ),
    rawData = new(
      'RawData',
      address = system.file('extdata/raw/p1_DipolDipol_SW-NE.dat', package = 'geoelectrics')
    ),
    measurementType = 'DipoleDipole',
    gpsCoordinates = new(
      'GpsCoordinates',
      address = system.file('extdata/gps/p1.txt', package = 'geoelectrics')
    )
  )

readline(prompt = 'Hit <Return> to read in next profiles:')

p2 <-
  new(
    'Profile',
    title = 'Profile 2',
    processedData = new(
      'ProcessedData',
      address = system.file('extdata/processed/p2_DipolDipol_SSW-NNE.xyz',
                            package = 'geoelectrics')
    ),
    rawData = new(
      'RawData',
      address = system.file('extdata/raw/p2_DipolDipol_SSW-NNE.dat', package = 'geoelectrics')
    ),
    measurementType = 'DipoleDipole',
    gpsCoordinates = new(
      'GpsCoordinates',
      address = system.file('extdata/gps/p2.txt', package = 'geoelectrics')
    )
  )

p3 <-
  new(
    'Profile',
    title = 'Profile 3',
    processedData = new(
      'ProcessedData',
      address = system.file('extdata/processed/p3_DipolDipol_S-N.xyz',
                            package = 'geoelectrics')
    ),
    rawData = new(
      'RawData',
      address = system.file('extdata/raw/p3_DipolDipol_S-N.dat', package = 'geoelectrics')
    ),
    measurementType = 'DipoleDipole',
    gpsCoordinates = new(
      'GpsCoordinates',
      address = system.file('extdata/gps/p3.txt', package = 'geoelectrics')
    )
  )

# Alter the height of profile 3 since it differs systematically from the other profiles
readline(prompt = 'Hit <Return> to alter the height of profile 3 since it differs systematically from the other profiles:')

p3 <- adjustHeight(p3, -10)

readline(prompt = 'Hit <Return> to create a profile set:') 

sinkhole <- new('ProfileSet',
                profiles = list(p1, p2, p3),
                title = 'Sinkhole')

# This profile set can also be loaded using 
# data(sinkhole)

# Plot raw data of profile 1
readline(prompt = 'Hit <Return> to plot the raw data:')

plotRaw(sinkhole@profiles[[1]])

plotRawHeight(sinkhole@profiles[[1]])

levelplotRaw(sinkhole@profiles[[1]])
levelplotLegendLabel()

# Plot processed data of profile 1
plotXyz(sinkhole@profiles[[1]])

plotXyzHeight(sinkhole@profiles[[1]])

levelplotXyz(sinkhole@profiles[[1]])
levelplotLegendLabel()

levelplotXyzHeight(sinkhole@profiles[[1]])
levelplotLegendLabel()

readline(prompt = 'Hit <Return> to plot the processed data in three dimensions:')

plot3d(sinkhole@profiles[[1]])

plot3d(sinkhole,
          xlab = 'length [m]',
          ylab = 'height above sea level [m]',
          zlab = 'length [m]')

# Plot legend for a single profile
plot.new()
plotLegend(sinkhole@profiles[[3]], horizontal = F)

# Plot legend for the profile set
plot.new()
plotLegend(sinkhole)

# Compare values on the intersection line between two profiles
plotIntersect(sinkhole)
plotIntersect(sinkhole@profiles[[1]], sinkhole@profiles[[2]])