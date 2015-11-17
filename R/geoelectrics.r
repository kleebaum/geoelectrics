# 3D-Presentation of Geoelectric Profiles Version 1.1
# Anja Kleebaum

### looking for working directory
whereFrom=as.character(sys.calls()[[1]][2]) 

### load packages
library(lattice) # for levelplots
library(rgl)

### change working directory to location of this file
try(setwd(dirname(whereFrom)))

###----Classes and constructors----####
#' A class to handle geoelectrics raw data
#'
#' @param address address of the raw data ascii file
#' @slot seaLevel data frame that contains raw data positions and resitance values 
#' @export
#' @examples 
#' rawDataObject <- new("RawData", address="../example/rawdata/p1_DipolDipol_SW-NE.dat")
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

#' A class to handle gps coordinates
#'
#' @param address address of the gps ascii file
#' @slot exact data frame that contains measured gps coordinates
#' @slot lm linear model of the measured gps coordinates
#' @slot relative relative coordinates
#' @slot lmRelative linear model of relative coordinates  
#' @export
#' @examples
#' new("GpsCoordinates", address="../example/gps/p1.txt")
setClass("GpsCoordinates",
         representation = representation(
           address = "character",
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

#' A class to handle xyz data
#' 
#' The software Res2DInv produces .xyz-files that contain the
#' inverted resistance values. This class parses this kind of files.
#'
#' @param address address of the xyz ascii file
#' @slot seaLevel data frame that contains positions and values withouth height adjustment
#' @slot heightAdaption data frame that contains positions and values after height adjustment 
#' @export
#' @examples new("XyzData", address="../example/xyzFiles/p1_DipolDipol_SW-NE.xyz")
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

#' A class to handle a single profile
#'
#' @param number number of the profile
#' @param xyzData object of XyzData class
#' @param rawData object of rawData class
#' @param measurementType type of measurement (e.g. Dipole Dipole, Wenner, ...)
#' @param gpsCoordinates object of GpsCoordinates class
#' @export
#' @examples
#' new("Profile", 
#'      number = 1,
#'      xyzData = 
#'        new("XyzData", 
#'             address = "../example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
#'      rawData = 
#'        new("RawData",
#'             address ="../example/rawdata/p1_DipolDipol_SW-NE.dat"),
#'      measurementType = "DipolDipol",
#'      gpsCoordinates = 
#'        new("GpsCoordinates",
#'             address ="../example/gps/p1.txt"))
setClass("Profile",
         representation = representation(
           number = "numeric",
           xyzData = "XyzData",
           rawData = "RawData", 
           measurementType = "character",
           gpsCoordinates = "GpsCoordinates"),
         prototype = prototype(
           number = 0))

#' A class to handle a collection of many profiles
#'
#' @param title title to plot
#' @param profiles list that contains objects of class Profile
#' @slot minLat minimum latitude value of all profiles
#' @slot minLon minimum longitude value of all profiles
#' @slot minData minimum data value of all profiles
#' @slot maxData maximum data value of all profiles
#' @export
setClass("ProfileSet",
         representation = representation(
           profiles = "list",
           title = "character",
           minLat = "numeric",
           minLon = "numeric",
           minData = "numeric",
           maxData = "numeric"))
setMethod("initialize", "ProfileSet",
          function(.Object, profiles=list(), title="") {
            .Object@profiles <- profiles
            .Object@title <- title
            
            minData <- 9999999
            maxData <- 0  
            minLat <- 100000000000
            minLon <- 100000000000            
            
            for (profile in profiles) {
              minDataX <- min(log(profile@xyzData@seaLevel$val))
              maxDataX <- max(log(profile@xyzData@seaLevel$val))
              if(minDataX < minData) minData <- minDataX
              if(maxDataX > maxData) maxData <- maxDataX
              
              minLatX <- min(profile@gpsCoordinates@exact$lat)
              minLonX <- min(profile@gpsCoordinates@exact$lon)
              if(minLatX < minLat) minLat <- minLatX
              if(minLonX < minLon) minLon <- minLonX
            }
            
            .Object@minLat <- minLat
            .Object@minLon <- minLon
            .Object@minData <- minData
            .Object@maxData <- maxData
            
            minLat <<- minLat
            minLon <<- minLon
            minData <<- minData
            maxData <<- maxData
            
            for(profile in profiles) {
              # latitude and longitude
              if(max(profile@gpsCoordinates@exact$lat) < 180) {
                # grad
                profile.m <- data.frame(lat=(profile@gpsCoordinates@exact$lat-minLat)*111000,
                                        lon=(profile@gpsCoordinates@exact$lon-minLon)*72000)
              }
              else {
                # utm
                profile.m <- data.frame(lat=(profile@gpsCoordinates@exact$lat-minLat),
                                        lon=(profile@gpsCoordinates@exact$lon-minLon))
              }     
              .Object@profiles[[profile@number]]@gpsCoordinates@relative <- profile.m
              .Object@profiles[[profile@number]]@gpsCoordinates@lmRelative <- lm(profile.m$lat ~ profile.m$lon)
            }
            return(.Object)
          })

###---Plotting functions---####   
#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data. 
#' @param Profile profile.
#' @export
plotXyz <- function(Profile) {
  plot(Profile@xyzData@seaLevel$dist, Profile@xyzData@seaLevel$depth, 
       xlab="Laenge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
}

#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the xyz 
#' data without height adjustment.
#' @param Profile profile.
#' @export
levelplotXyz <- function(Profile) {
  levelplot(log(Profile@xyzData@seaLevel$val) ~ Profile@xyzData@seaLevel$dist * Profile@xyzData@seaLevel$depth, 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "ohne Hoehenanpassung"))
}

#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data
#' after height adjustment. 
#' @param Profile profile.
#' @export
plotXyzHeight <- function(Profile) {
  plot(data.frame(Profile@xyzData@heightAdaption$dist, Profile@xyzData@heightAdaption$depth), 
       xlab="Laenge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
}

#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the 
#' xyz data after height adjustment.
#' @param Profile profile.
#' @export
levelplotXyzHeight <- function(Profile) {
  levelplot(log(Profile@xyzData@heightAdaption$val) ~ round(Profile@xyzData@heightAdaption$dist) * round(Profile@xyzData@heightAdaption$depth), 
            col.regions = colorRampPalette(colors), interpolate=F, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "mit Hoehenanpassung"))
}

#' Plot raw data levels
#' 
#' Plots the interpolated resistance values of the
#' raw data without height adjustment. 
#' @param Profile profile.
#' @export
levelplotRaw <- function(Profile) {
  levelplot(round(log(Profile@rawData@seaLevel$val)) ~ round(Profile@rawData@seaLevel$dist) * round(-1*Profile@rawData@seaLevel$depth), 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), aspect="iso",
            panel = lattice.getOption("panel.levelplot"))
}

#' Plot raw points
#' 
#' Plots points of the raw data to show measurement gaps. 
#' @param Profile profile.
#' @export
plotRaw <- function(Profile) {  
  plot(Profile@rawData@seaLevel$dist, -1*(Profile@rawData@seaLevel$depth), 
       xlab="Länge [m]", ylab="Tiefe [m]", 
       main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), asp=1) 
}

# plot3dXyz <- function(ProfileSet) {
#   #lapply(ProfileSet$profiles, plot3dXyz)
#   for (profile in ProfileSet@profiles)
#     plot3dXyz(profile)
# }

setMethod("plot3dXyz", "ProfileSet",
          function(ProfileSet) {
            lapply(ProfileSet@profiles, plot3dXyz)
          })

#' Plots profiles 3D
#' 
#' Plots the interpolated resistance values of the 
#' xyz data for all profiles.
#' @param Profile profile.
#' @export
setMethod("plot3dXyz", "Profile",
          function(Profile) {
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
  rgl.texts(y[1], Profile@xyzData@heightAdaption$depth[1]+20, x[1], 
            text=paste("Profil", Profile@number), cex=1, color="black")
  axes3d(edges="bbox",  yunit=25, expand=1.2)
  #title3d('main','sub','xlab','ylab','zlab')
  #title3d('Sinkhole','','Strecke [m]','Hoehe ueber NN [m]','Strecke [m]')
  #title3d('','','[m]','','[m]')
  #title3d('','','','Hoehe [m]','')
})

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
