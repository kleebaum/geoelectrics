# 3D-Presentation of Geoelectric Profiles Version 1.1
# Anja Kleebaum

### looking for working directory
whereFrom=as.character(sys.calls()[[1]][2]) 

### load packages
library(lattice) # for levelplots
library(rgl)

### change working directory to right one
try(setwd(dirname(whereFrom)))

###----Classes----####
setClass("RawData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame"))
setMethod("initialize", "RawData",
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("Raw Data address is missing.")
            } else {
              con  <- file(address, open = "r")
              
              skipLines1 <- 9
              skipLines2 <- 0
              numberOfRows1 <- 0
              numberOfRows2 <- 0
              
              for(i in 1:10) {
                oneLine <- readLines(con, n=1)
              }
              
              while(grepl(".", oneLine, fixed=T)) {
                oneLine <- readLines(con, n=1)
                numberOfRows1 <- numberOfRows1 + 1
              }
              
              profile <- read.table(file=address, skip=skipLines1, 
                                    header=F, nrows=numberOfRows1)
              
              .Object@seaLevel <- data.frame(
                "dist"=profile[1],
                "depth"=profile[2],
                "val"=profile[4])
              colnames(.Object@seaLevel) <- c("dist", "depth", "val")
              
              close(con)              
            }
            return(.Object)
          })

setClass("GpsCoordinates",
         representation = representation(
           address = "character",
           rawData = "data.frame",
           exact = "data.frame",
           lm = "lm",
           relative = "data.frame",
           lmRelative = "lm"),
         prototype = prototype(
           lm = lm(1~1),
           lmRelative = lm (1~1)))
setMethod("initialize", "GpsCoordinates", 
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("GPS coordinates address is missing.")
            } else {
              gpsData <- read.table(file=address, header=T)               
              .Object@exact <- data.frame(
                "lat"=gpsData[1],
                "lon"=gpsData[2])
              colnames(.Object@exact) <- c("lat", "lon")
              .Object@lm <- lm(.Object@exact$lat ~ .Object@exact$lon)
            }
            return(.Object)
          })

setClass("XyzData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame",
           heightAdaption = "data.frame"))
setMethod("initialize", "XyzData",
          function(.Object, address) {
            if(nchar(address) == 0) {
              print("XYZ data address is missing.")
            } else {
              con  <- file(address, open = "r")
              
              skipLines1 <- 0
              numberOfRows <- 0
              numberOfRows2 <- 0
              
              oneLine <- readLines(con, n=1)
              while(grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                skipLines1 <- skipLines1 + 1
              }
              
              while(!grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                numberOfRows <- numberOfRows + 1    
              }
              
              skipLines2 <- skipLines1 + numberOfRows
              
              while(grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                skipLines2 <- skipLines2 + 1
              }
              
              while(!grepl("/", oneLine)) {
                oneLine <- readLines(con, n=1)
                numberOfRows2 <- numberOfRows2 + 1    
              }
              
              profile_without_topo <- read.table(file=address, skip=skipLines1, 
                                                 header=F, nrows=numberOfRows)
              .Object@seaLevel <- data.frame(
                "dist"=profile_without_topo[1],
                "depth"=profile_without_topo[2],
                "val"=profile_without_topo[3])
              colnames(.Object@seaLevel) <- c("dist", "depth", "val")
              
              profile <- read.table(file=address, skip=skipLines2, 
                                    header=F, nrows=numberOfRows2)
              
              .Object@heightAdaption <- data.frame(
                "dist"=profile[1],
                "depth"=profile[2],
                "val"=profile[3])
              colnames(.Object@heightAdaption) <- c("dist", "depth", "val")
              
              close(con)
            }
            return(.Object)               
          })

setClass("Profile",
         representation = representation(
           number = "numeric",
           xyzData = "XyzData",
           rawData = "RawData", 
           measurementType = "character",
           gpsCoordinates = "GpsCoordinates"),
         prototype = prototype(
           number = 0))

###----Getter and setter methods----####
setXyzAddress <- function(Profile, address) {
  Profile@xyzData@address <- address
  Profile <- parseXyzData(Profile)  
  return(Profile)
}

getXyzAddress <- function(Profile) {
  return(Profile@xyzData@address)
}

setRawAddress <- function(Profile, address) {
  Profile@rawData@address <- address
  Profile <- parseRawData(Profile)  
  return(Profile)
}

getRawAddress <- function(Profile) {
  return(Profile@rawData@address)
}

setGpsAddress <- function(Profile, address) {
  Profile@gpsCoordinates@address <- address
  Profile <- parseGpsData(Profile)  
  return(Profile)
}

getGpsAddress <- function(Profile) {
  return(Profile@gpsCoordinates@address)
}

findMinMaxValues <- function(Profiles) {
  minData <- 9999999
  maxData <- 0  
  minLat <- 100000000000
  minLon <- 100000000000
  
  for (Profile in Profiles) {
    minDataX <- min(log(Profile@xyzData@seaLevel$val))
    maxDataX <- max(log(Profile@xyzData@seaLevel$val))
    if(minDataX < minData) minData <- minDataX
    if(maxDataX > maxData) maxData <- maxDataX
    
    minLatX <- min(Profile@gpsCoordinates@exact$lat)
    minLonX <- min(Profile@gpsCoordinates@exact$lon)
    if(minLatX < minLat) minLat <- minLatX
    if(minLonX < minLon) minLon <- minLonX
  }  
  
  minLat <<- minLat
  minLon <<- minLon
  minData <<- minData
  maxData <<- maxData
}

calculateRelativeCoordinates <- function(Profile) {
    # latitude and longitude
    if(max(Profile@gpsCoordinates@exact$lat) < 180) {
      # grad
      profile.m <- data.frame(lat=(Profile@gpsCoordinates@exact$lat-minLat)*111000,
                              lon=(Profile@gpsCoordinates@exact$lon-minLon)*72000)
    }
    else {
      # utm
      profile.m <- data.frame(lat=(Profile@gpsCoordinates@exact$lat-minLat),
                              lon=(Profile@gpsCoordinates@exact$lon-minLon))
    }     
    slot(Profile@gpsCoordinates, "relative") <- profile.m
    slot(Profile@gpsCoordinates, "lmRelative") <- lm(profile.m$lat ~ profile.m$lon)
    return(Profile)
}

###---Plotting functions---####   
plotXyz <- function(Profile) {
  plot(Profile@xyzData@seaLevel$dist, Profile@xyzData@seaLevel$depth, 
       xlab="Laenge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
}

levelplotXyz <- function(Profile) {
  levelplot(log(Profile@xyzData@seaLevel$val) ~ Profile@xyzData@seaLevel$dist * Profile@xyzData@seaLevel$depth, 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "ohne Hoehenanpassung"))
}

plotXyzHeight <- function(Profile) {
  plot(data.frame(Profile@xyzData@heightAdaption$dist, Profile@xyzData@heightAdaption$depth), 
       xlab="Laenge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
}

levelplotXyzHeight <- function(Profile) {
  levelplot(log(Profile@xyzData@heightAdaption$val) ~ round(Profile@xyzData@heightAdaption$dist) * round(Profile@xyzData@heightAdaption$depth), 
            col.regions = colorRampPalette(colors), interpolate=F, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "mit Hoehenanpassung"))
}

levelplotRaw <- function(Profile) {
  levelplot(round(log(Profile@rawData@seaLevel$val)) ~ round(Profile@rawData@seaLevel$dist) * round(-1*Profile@rawData@seaLevel$depth), 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), aspect="iso",
            panel = lattice.getOption("panel.levelplot"))
}

plotRaw <- function(Profile) {  
  plot(Profile@rawData@seaLevel$dist, -1*(Profile@rawData@seaLevel$depth), 
       xlab="Länge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), asp=1) 
}

plot3dXyz <- function(Profile) {
  colorAssignment <- myColorRamp(colors, log(Profile@xyzData@heightAdaption$val))
  
  l <- Profile@xyzData@heightAdaption$dist # hypotenuse
  m <- Profile@gpsCoordinates@lmRelative$coefficients[2] # y = mx + n
  n <- Profile@gpsCoordinates@lmRelative$coefficients[1]
  alpha <- atan(m)
  
  # calculate adjacent leg
  x <- cos(alpha) * l
  
  # get starting point and adjust
  s.x <- min(Profile@gpsCoordinates@relative$lon)
  x <- x + s.x
  
  # calculate opposite leg
  y <- m*x + n
  
  # plot 3D    
  rgl.bg(color="white")
  points3d(y, Profile@xyzData@heightAdaption$depth, x, color=colorAssignment, size=pointsize)  
  rgl.bbox()  
  rgl.texts(y[1], Profile@xyzData@heightAdaption$depth[1]+5, x[1], 
            text=paste("Profil", Profile@number), cex=1, color="black")
  axes3d(edges="bbox",  yunit=25, expand=1.2)
  #title3d('main','sub','xlab','ylab','zlab')
  #title3d('Sinkhole','','Strecke [m]','Hoehe ueber NN [m]','Strecke [m]')
  #title3d('','','[m]','','[m]')
  #title3d('','','','Hoehe [m]','')
}  

heightAdjustment <- function(Profile, deltaMeter) {
  Profile@xyzData@heightAdaption$depth <- 
    Profile@xyzData@heightAdaption$depth + deltaMeter
  p[Profile@number] <- Profile
  p <<- p
}

###---Settings---####
pointsize <- 10
colors <- c("blue", "green", "yellow", "orange", "red", "purple")

myColorRamp <- function(colors, values) { # maps color to resistivity value
  v <- (values - minData)/diff(range(minData,maxData)) # same colors for all profiles
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
} 
