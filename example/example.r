# Script for plotting three profiles of an example measurement
# filled sinkhole

# a new object is created for each profile and stored within the list p
p1 <- new("Profile",
            title = "Profile 1",
            xyzData = 
              new("XyzData", 
                  address = "../example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
            rawData = 
              new("RawData",
                  address = "../example/rawdata/p1_DipolDipol_SW-NE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address = "../example/gps/p1.txt"))

p2 <- new("Profile",
            title = "Profile 2",
            xyzData = 
              new("XyzData", 
                  address = "../example/xyzFiles/p2_DipolDipol_SSW-NNE.xyz"),
            rawData = 
              new("RawData",
                  address = "../example/rawdata/p2_DipolDipol_SSW-NNE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address = "../example/gps/p2.txt")) 

p3 <- new("Profile",
            title = "Profile 3",
            xyzData = 
              new("XyzData", 
                  address = "../example/xyzFiles/p3_DipolDipol_S-N.xyz"),
            rawData = 
              new("RawData",
                  address = "../example/rawdata/p3_DipolDipol_S-N.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address = "../example/gps/p3.txt")) 

p3 <- heightAdjustment(p3, -10)

sampleProfileSet <- new("ProfileSet",
                       profiles = list(p1, p2, p3),
                       title="Sinkhole")

# plot3dXyz(sampleProfileSet,
#           xlab="length [m]", 
#           ylab="height above sea level [m]",
#           zlab="length [m]")

# plotLegend(sampleProfileSet)

plotIntersect(sampleProfileSet)
plotIntersect(sampleProfileSet@profiles[[1]], sampleProfileSet@profiles[[2]])

#save.image(file="sinkhole.RData", ascii = TRUE)
