# Script for plotting three profiles of an example measurement
# filled sinkhole

p <- list() # a list has to be initialized to combine all profiles

# a new object is created for each profile and stored within the list p
p[1] <- new("Profile", 
            number = 1,
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

p[2] <- new("Profile", 
            number = 2,
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

p[3] <- new("Profile", 
            number = 3,
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

heightAdjustment(p[[3]], -10)

exampleProfiles <- new("ProfileSet",
                       profiles = p,
                       title="Sinkhole")

plot3dXyzAll(exampleProfiles)

#save.image(file="eichig.RData", ascii = TRUE)