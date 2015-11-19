# 3D-Presentation of Geoelectric Profiles Version 1.1
# Anja Kleebaum

### looking for working directory
whereFrom=as.character(sys.calls()[[1]][2]) 

### load packages
library(lattice) # for levelplots
library(rgl)
library(fields)

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
           title = "character",
           number = "numeric",
           xyzData = "XyzData",
           rawData = "RawData", 
           measurementType = "character",
           gpsCoordinates = "GpsCoordinates"),
         prototype = prototype(
           number = 0,
           title = ""))

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
          function(.Object, profiles=list(), title="",
                   minData=9999999, maxData=0,
                   minLat=100000000000, minLon=100000000000) {
            .Object@profiles <- profiles
            .Object@title <- title         
            
            for (profile in profiles) {
              minDataX <- min(trafo(profile@xyzData@seaLevel$val))
              maxDataX <- max(trafo(profile@xyzData@seaLevel$val))
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
            
            number <- 1
            for(profile in profiles) {
              .Object@profiles[[number]]@number <- number
              
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
              .Object@profiles[[number]]@gpsCoordinates@relative <- profile.m
              .Object@profiles[[number]]@gpsCoordinates@lmRelative <- lm(profile.m$lat ~ profile.m$lon)
              
              number <- number + 1
            }
            return(.Object)
          })

###---Plotting functions---####   
#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data. 
#' @param Profile profile
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotXyz <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {
  plot(Profile@xyzData@seaLevel$dist, Profile@xyzData@seaLevel$depth, 
       xlab=xlab, ylab=ylab, main=main, asp=1)
}

#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the xyz 
#' data without height adjustment.
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
levelplotXyz <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {
  levelplot(trafo(Profile@xyzData@seaLevel$val) ~ Profile@xyzData@seaLevel$dist * Profile@xyzData@seaLevel$depth, 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab=xlab, ylab=ylab, main=main)
}

#' Plot Xyz points
#' 
#' Plots the interpolated points of the xyz data
#' after height adjustment. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotXyzHeight <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "with topography")) {
  plot(data.frame(Profile@xyzData@heightAdaption$dist, Profile@xyzData@heightAdaption$depth), 
       xlab=xlab, ylab=ylab, main=main, asp=1)
}

#' Plot levels of xyz data
#' 
#' Plots the interpolated resistance values of the 
#' xyz data after height adjustment.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @param Profile profile.
#' @export
levelplotXyzHeight <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "with topography")) {
  levelplot(trafo(Profile@xyzData@heightAdaption$val) ~ round(Profile@xyzData@heightAdaption$dist) * round(Profile@xyzData@heightAdaption$depth), 
            col.regions = colorRampPalette(colors), interpolate=F, 
            regions=T, xlab=xlab, ylab=ylab, main=main)
}

#' Plot raw data levels
#' 
#' Plots the interpolated resistance values of the
#' raw data without height adjustment.
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
levelplotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {
  levelplot(round(trafo(Profile@rawData@seaLevel$val)) ~ round(Profile@rawData@seaLevel$dist) * round(-1*Profile@rawData@seaLevel$depth), 
            col.regions = colorRampPalette(colors), interpolate=T, 
            regions=T, xlab=xlab, ylab=ylab, main=main, aspect="iso",
            panel = lattice.getOption("panel.levelplot"))
}

#' Plot raw points
#' 
#' Plots points of the raw data to show measurement gaps. 
#' @param Profile profile.
#' @param xlab label for x-axes
#' @param ylab label for y-axes
#' @param main title to be plotted
#' @export
plotRaw <- function(Profile, xlab="Length [m]", ylab="Depth [m]",
                    main=paste(Profile@title, "without topography")) {  
  plot(Profile@rawData@seaLevel$dist, -1*(Profile@rawData@seaLevel$depth), 
       xlab=xlab, ylab=ylab, main=main, asp=1) 
}

#' Plots profiles 3D
#' 
#' Plots the interpolated resistance values of the 
#' xyz data for all profiles.
#' 
#' @param .Object either a single Profile or a ProfileSet
#' @param title title to be plotted
#' @param sub subtitle to be plotted
#' @param xlab label of the x-axes, e.g. length [m]
#' @param ylab label of the y-axes, e.g. height above sea level [m]
#' @param zlab label of the z-axes, e.g. length [m]
#' @export
setGeneric("plot3dXyz", function(.Object, title="", sub="",
                                 xlab="", ylab="", zlab=""){
  standardGeneric("plot3dXyz")
})

#' @rdname plot3dXyz
#' @aliases plot3d
#' @export
setMethod("plot3dXyz", signature(.Object="ProfileSet"),
          function(.Object, title=.Object@title, sub="",
                   xlab="", ylab="", zlab="") {
            lapply(.Object@profiles, plot3dXyz)
            title3d(title, sub, xlab, ylab, zlab)
          })

#' @rdname plot3dXyz
#' @aliases plot3d
#' @export
setMethod("plot3dXyz", signature(.Object="Profile"),
          function(.Object, title="", sub="",
                   xlab="", ylab="", zlab="") {
            title3d(title, sub, xlab, ylab, zlab)
            colorAssignment <- myColorRamp(colors, trafo(.Object@xyzData@heightAdaption$val))
            
            l <- .Object@xyzData@heightAdaption$dist # hypotenuse
            m <- .Object@gpsCoordinates@lmRelative$coefficients[2] # y = mx + n
            n <- .Object@gpsCoordinates@lmRelative$coefficients[1]
            alpha <- atan(m)
            
            # calculate adjacent leg
            x <- cos(alpha) * l
            
            # get starting point and adjust
            start.x <- min(.Object@gpsCoordinates@relative$lon)
            x <- x + start.x
            
            # calculate opposite leg
            y <- m * x + n
            
            # plot 3D    
            rgl.bg(color="white")
            points3d(y, .Object@xyzData@heightAdaption$depth, x, color=colorAssignment, size=pointsize)  
            rgl.bbox()  
            rgl.texts(y[1], .Object@xyzData@heightAdaption$depth[1]+20, x[1], 
                      text=paste(.Object@title), cex=1, color="black")
            axes3d(edges="bbox", yunit=25, expand=1.2)
          })

setGeneric("plotLegend", function(.Object) {
  standardGeneric("plotLegend")
})

setMethod("plotLegend", signature(.Object="ProfileSet"),
          function(.Object) {
            image.plot(legend.only=TRUE, zlim= c(minData, maxData), 
                       legend.lab = "resistivity",
                       nlevel=128, col=colorRampPalette(colors)(127))
          })

setMethod("plotLegend", signature(.Object="Profile"),
          function(.Object) {
            image.plot(legend.only=TRUE, zlim= c(minData, maxData),
                       legend.lab = "resistivity",
                       nlevel=128, col=colorRampPalette(colors)(127))
          })

setGeneric("plotIntersect", function(.Object1, .Object2=NULL, 
                                     xlab="Length [m]", ylab="Resistivity", main="") {
  standardGeneric("plotIntersect")  
})

setMethod("plotIntersect", signature(.Object1="ProfileSet"),
          function(.Object1) {
            for(i in 1:(length(.Object1@profiles)-1)) 
              for(j in (i+1):length(.Object1@profiles))
                plotIntersect(.Object1@profiles[[i]], .Object1@profiles[[j]])
})

setMethod("plotIntersect", signature(.Object1="Profile", .Object2="Profile"),
          function(.Object1, .Object2, xlab, ylab, main) {
            # slopes m
            m1 <- .Object1@gpsCoordinates@lmRelative$coefficients[2]
            m2 <- .Object2@gpsCoordinates@lmRelative$coefficients[2]
            
            # intercepts n
            n1 <- .Object1@gpsCoordinates@lmRelative$coefficients[1]
            n2 <- .Object2@gpsCoordinates@lmRelative$coefficients[1]
            
            # calculate intersection point
            # m1 * x.intersect + n1 = m2 * x.intersect + n2
            x.intersect <- (n2 - n1)/(m1 - m2)
            y.intersect <- m1 * x.intersect + n1

            # starting points of Profile 1 and 2
            x.start1 <- min(.Object1@gpsCoordinates@relative$lon)
            y.start1 <- m1 * x.start1 + n1            
            x.start2 <- min(.Object2@gpsCoordinates@relative$lon)
            y.start2 <- m2 * x.start2 + n2
            
            # calculate length (hypotenuse) from starting to intersection point
            x.diff1 <- x.intersect - x.start1
            y.diff1 <- y.intersect - y.start1
            x.diff2 <- x.intersect - x.start2
            y.diff2 <- y.intersect - y.start2
            
            length1 <- sqrt(x.diff1^2 + y.diff1^2)
            length2 <- sqrt(x.diff2^2 + y.diff2^2)

            # identify point indices on intersection line and next to it            
            indices1 <- c(which(round(.Object1@xyzData@heightAdaption$dist) == round(length1)),
                          which(round(.Object1@xyzData@heightAdaption$dist) == round(length1 + 1)),
                          which(round(.Object1@xyzData@heightAdaption$dist) == round(length1 - 1)))
            
            indices2 <- c(which(round(.Object2@xyzData@heightAdaption$dist) == round(length2, 0)),
                          which(round(.Object2@xyzData@heightAdaption$dist) == round(length2 + 1, 0)),
                          which(round(.Object2@xyzData@heightAdaption$dist) == round(length2 - 1, 0)))
            
            # identify xyz values for these indices
            res1 <- data.frame(
              "dist" = .Object1@xyzData@heightAdaption$dist[indices1],
              "depth" = .Object1@xyzData@heightAdaption$depth[indices1],
              "val" = .Object1@xyzData@heightAdaption$val[indices1])
            
            res2 <- data.frame(
              "dist" = .Object2@xyzData@heightAdaption$dist[indices2],
              "depth" = .Object2@xyzData@heightAdaption$depth[indices2],
              "val" = .Object2@xyzData@heightAdaption$val[indices2])
            
            #boxplot(trafo(res1$val)~round(res1$depth))
            plot(res1$depth, trafo(res1$val),
                 xlim=c(min(res1$depth, res2$depth), max(res1$depth, res2$depth)),
                 ylim=c(trafo(min(res1$val, res2$val)), trafo(max(res1$val, res2$val))),
                 xlab=xlab, ylab=ylab, main=main)
            points(res2$depth, trafo(res2$val))            
})

#' Method to adjust height of a single profile
#' 
#' GPS measurements might differ otherwise.
#' 
#' @param Profile a single Profile
#' @param deltaMeter positive or negative value
#' @export
heightAdjustment <- function(Profile, deltaMeter) {
  Profile@xyzData@heightAdaption$depth <- 
    Profile@xyzData@heightAdaption$depth + deltaMeter
  return(Profile)
}

###---Settings---####
pointsize <- 10
colors <- c("blue", "green", "yellow", "orange", "red", "purple")

logBase <- 10
trafo <- function(x, base=logBase) {
  log(x, base)
}

myColorRamp <- function(colors, values) { # maps color to resistivity value
  v <- (values - minData)/diff(range(minData,maxData)) # same colors for all profiles
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
} 
