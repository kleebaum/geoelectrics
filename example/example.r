# Script for plotting three profiles of an example measurement
# Sinkhole

p <- list() # a list has to be initialized to combine all profiles

# a new object is created for each profile and stored within the list p
p[1] <- new("Profile", 
            number = 1,
            xyzData = 
              new("XyzData", 
                  address = "example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
            rawData = 
              new("RawData",
                  address ="example/rawdata/p1_DipolDipol_SW-NE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="example/gps/p1.txt"))

p[2] <- new("Profile", 
            number = 2,
            xyzData = 
              new("XyzData", 
                  address = "example/xyzFiles/p2_DipolDipol_SSW-NNE.xyz"),
            rawData = 
              new("RawData",
                  address ="example/rawdata/p2_DipolDipol_SSW-NNE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="example/gps/p2.txt")) 

p[3] <- new("Profile", 
            number = 3,
            xyzData = 
              new("XyzData", 
                  address = "example/xyzFiles/p3_DipolDipol_S-N.xyz"),
            rawData = 
              new("RawData",
                  address ="example/rawdata/p3_DipolDipol_S-N.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="example/gps/p3.txt")) 

# Nullpunkte fuer 3D Darstellung
#findMinMaxValues(p)

minLon <- 653737
minLat <- 5547494

for(x in p) { 
  p[x@number] <- parseXyzData(p[[x@number]])
  p[x@number] <- parseGpsData(p[[x@number]])
  try(p[x@number] <- parseRawData(p[[x@number]]))
}

heightAdjustment(p[[3]], -10)

minData <- 9999999
maxData <- 0

for(x in p) {
  minDataX <- min(log(p[[x@number]]@xyzData@seaLevel$V3))
  maxDataX <- max(log(p[[x@number]]@xyzData@seaLevel$V3))
  if(minDataX < minData) minData <- minDataX
  if(maxDataX > maxData) maxData <- maxDataX
}

#save.image(file="eichig.RData", ascii = TRUE)