# Script for plotting an example measurement
# Doline bei Eichig
p <- list()

p[1] <- new("Profile", 
            number = 1,
            xyzData = 
              new("XyzData", 
                  address = "ge3dExample2/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
            rawData = 
              new("RawData",
                  address ="ge3dExample2/rawdata/p1_DipolDipol_SW-NE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="ge3dExample2/gps/p1.txt"))

p[2] <- new("Profile", 
            number = 2,
            xyzData = 
              new("XyzData", 
                  address = "ge3dExample2/xyzFiles/p2_DipolDipol_SSW-NNE.xyz"),
            rawData = 
              new("RawData",
                  address ="ge3dExample2/rawdata/p2_DipolDipol_SSW-NNE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="ge3dExample2/gps/p2.txt")) 

p[3] <- new("Profile", 
            number = 3,
            xyzData = 
              new("XyzData", 
                  address = "ge3dExample2/xyzFiles/p3_DipolDipol_S-N.xyz"),
            rawData = 
              new("RawData",
                  address ="ge3dExample2/rawdata/p3_DipolDipol_S-N.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address ="ge3dExample2/gps/p3.txt")) 

# Nullpunkte fuer 3D Darstellung
minLon <- 653737
minLat <- 5547494

for(x in p) { 
  p[x@number] <- getXyzData(p[[x@number]])
  p[x@number] <- getGpsData(p[[x@number]])
  try(p[x@number] <- getRawData(p[[x@number]]))
}

heightAdjustment(p[[3]], -10)

#save.image(file="eichig.RData", ascii = TRUE)