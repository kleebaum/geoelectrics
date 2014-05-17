# 3D-Presentation of Geoelectric Profiles Version 1.1
# Anja Kleebaum

### looking for working directory
whereFrom=sys.calls()[[1]]
whereFrom=as.character(whereFrom[2]) 
whereFrom=paste(getwd(),whereFrom,sep="/") # prefix it with the current working directory

### install packages
require(tcltk)
library(tkrplot)
library(lattice) # for levelplots
library(rgl)
tt <- tktoplevel()
tkwm.minsize(tt, 400, 300)
tktitle(tt) <- "Kleelectrics: 3D-Presentation of Geoelectric Profiles"

### change working directory to right one
try(setwd(dirname(whereFrom)))

###----Classes and methods----####
setClass("RawData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame"))

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

setClass("XyzData",
         representation = representation(
           address = "character",
           seaLevel = "data.frame",
           heightAdaption = "data.frame"))

setClass("Profile",
         representation = representation(
           number = "numeric",
           xyzData = "XyzData",
           rawData = "RawData", 
           measurementType = "character",
           gpsCoordinates = "GpsCoordinates"),
         prototype = prototype(
           number = 0))

### class methods
setXyzAddress <- function(Profile, address) {
  Profile@xyzData@address <- address
  Profile <- getXyzData(Profile)  
  return(Profile)
}

setRawAddress <- function(Profile, address) {
  Profile@rawData@address <- address
  Profile <- getRawData(Profile)  
  return(Profile)
}

setGpsAddress <- function(Profile, address) {
  Profile@gpsCoordinates@address <- address
  Profile <- getGpsData(Profile)  
  return(Profile)
}

getXyzData <- function(Profile) {
  if(length(Profile@xyzData@address) == 0) {
    print("address missing")
  } else {
    con  <- file(Profile@xyzData@address, open = "r")
    
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
    
    profile_without_topo <- read.table(file=Profile@xyzData@address, skip=skipLines1, header=F, nrows=numberOfRows)
    slot(Profile@xyzData, "seaLevel") <- data.frame(
      profile_without_topo[1],profile_without_topo[2],
      profile_without_topo[3])#,profile_without_topo[5])
    
    profile <- read.table(file=Profile@xyzData@address, skip=skipLines2, header=F, nrows=numberOfRows2)
    
    slot(Profile@xyzData, "heightAdaption") <- data.frame(
      profile[1],profile[2],profile[3])#,profile[5])
    
    close(con)
    
    return(Profile)
  }
}

getRawData <- function(Profile) {
  if(length(Profile@rawData@address) == 0) {
    print("address missing")
  } else {
    con  <- file(Profile@rawData@address, open = "r")
    
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
    
    skipLines2 <- skipLines1 + numberOfRows1
    
    while(!grepl(".", oneLine, fixed=T)) {
      oneLine <- readLines(con, n=1)
      skipLines2 <- skipLines2 + 1
    }
    
    while(grepl(".", oneLine, fixed=T)) {
      oneLine <- readLines(con, n=1)
      numberOfRows2 <- numberOfRows2 + 1
    }
    
    profile <- read.table(file=Profile@rawData@address, skip=skipLines1, header=F, nrows=numberOfRows1)
    height <- read.table(file=Profile@rawData@address, skip=skipLines2, header=F, nrows=numberOfRows2)
    
    slot(Profile@rawData, "seaLevel") <- data.frame(
      profile[1],profile[2],
      profile[4])
    
    close(con)
    
    return(Profile)
  }
}

getGpsData <- function(Profile) {
  # height from adjusted rawdata file
  if(length(Profile@rawData@address) == 0) {
    print("address missing (rawdata)")
  } else {
    
  }
  
  # latitude and longitude
  if(length(Profile@gpsCoordinates@address) == 0) {
    print("address missing (gps-file)")
  } else {
    gpsData <- read.table(file=Profile@gpsCoordinates@address, header=T) 
    
    slot(Profile@gpsCoordinates, "exact") <- data.frame(
      gpsData[1],gpsData[2])
    
    lm.profile <- lm(Profile@gpsCoordinates@exact$lat ~ Profile@gpsCoordinates@exact$lon)
    slot(Profile@gpsCoordinates, "lm") <- lm.profile
    
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
    
    lm.profile.relative <- lm(Profile@gpsCoordinates@relative$lat ~ Profile@gpsCoordinates@relative$lon)
    
    slot(Profile@gpsCoordinates, "lmRelative") <- lm.profile.relative
    
    return(Profile)
  }
}

###---Plotting function---####         
plotXyz <- function(Profile) {
  try(tkgrid.remove(img))
  assign("img", tkrplot(frameRight, function() {
    plot(Profile@xyzData@seaLevel$V1, Profile@xyzData@seaLevel$V2, 
         xlab="Laenge [m]", ylab="Tiefe [m]", 
         main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
  }), envir = .GlobalEnv)
  tkgrid(img)
}

levelplotXyz <- function(Profile) {
  #values <- log(Profile@xyzData@seaLevel$V3)
  #v <- (values - min(values))/diff(range(values))
  assign("img", levelplot(log(Profile@xyzData@seaLevel$V3) ~ Profile@xyzData@seaLevel$V1 * Profile@xyzData@seaLevel$V2, 
            col.regions = colorRampPalette(colors), interpolate=T, 
            #col.regions = colorRamp(colors)(values), interpolate=T, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), aspect="iso"),
         envir=.GlobalEnv)
  source("savePlots.r", echo=T)
}

plotXyzHeight <- function(Profile) {
  try(tkgrid.remove(img))
  assign("img", tkrplot(frameRight, function() {
    plot(data.frame(Profile@xyzData@heightAdaption$V1, Profile@xyzData@heightAdaption$V2), 
         xlab="Laenge [m]", ylab="Tiefe [m]", 
         main=paste("Profil", Profile@number, "mit Hoehenanpassung"), asp=1)
  }), envir = .GlobalEnv)
  tkgrid(img)
}

levelplotXyzHeight <- function(Profile) {
  assign("img", levelplot(log(Profile@xyzData@heightAdaption$V3) ~ round(Profile@xyzData@heightAdaption$V1) * round(Profile@xyzData@heightAdaption$V2), 
            col.regions = colorRampPalette(colors), interpolate=F, 
            regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
            main=paste("Profil", Profile@number, "mit Hoehenanpassung"), aspect="iso"),
         envir=.GlobalEnv)
  source("savePlots.r", echo=T)
}

levelplotRaw <- function(Profile) {
  assign("img", levelplot(round(log(Profile@rawData@seaLevel$V4)) ~ round(Profile@rawData@seaLevel$V1) * round(-1*Profile@rawData@seaLevel$V2), 
                   col.regions = colorRampPalette(colors), interpolate=T, 
                   regions=T, xlab="Laenge [m]", ylab="Tiefe [m]", 
                   main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), aspect="iso",
                   panel = lattice.getOption("panel.levelplot")),
         envir = .GlobalEnv)
  source("savePlots.r", echo=T)
}

plotRaw <- function(Profile) {  
  try(tkgrid.remove(img))
  assign("img", tkrplot(frameRight, function() {
    plot(Profile@rawData@seaLevel$V1, -1*(Profile@rawData@seaLevel$V2), 
         xlab="Länge [m]", ylab="Tiefe [m]", 
         main=paste("Profil", Profile@number, "ohne Hoehenanpassung"), asp=1)
  }), envir = .GlobalEnv)  
  tkgrid(img)  
}

plot3dXyz <- function(Profile) {
  colorAssignment <- myColorRamp(colors, log(Profile@xyzData@heightAdaption$V3))
  
  l <- Profile@xyzData@heightAdaption$V1 # length of profile 
  m <- Profile@gpsCoordinates@lmRelative$coefficients[2] # y = mx + n
  n <- Profile@gpsCoordinates@lmRelative$coefficients[1]
  alpha <- atan(m)
  
  # Berechung von x und y ueber Winkel
  x <- cos(alpha) * l
  y <- sin(alpha) * l
  
  # Anpassung Startpunkte
  s.y <- Profile@gpsCoordinates@relative$lat[1]
  s.x <- Profile@gpsCoordinates@relative$lon[1]
  
  y <- y + s.y
  x <- x + s.x
  
  # Plot 3D    
  rgl.bg(color="white")
  points3d(y, Profile@xyzData@heightAdaption$V2, x, color=colorAssignment, size=pointsize)  
  rgl.bbox()  
  rgl.texts(y[1], Profile@xyzData@heightAdaption$V2[1]+5, x[1], 
            text=paste("Profil", Profile@number), cex=1, color="black")
  axes3d(edges="bbox",  yunit=25, expand=1.2)
  #title3d('main','sub','xlab','ylab','zlab')
  #title3d('Geoelektrik Eichig 2013','','Strecke [m]','Hoehe ueber NN [m]','Strecke [m]')
  #title3d('','','[m]','','[m]')
  #title3d('','','','Hoehe [m]','')
}  

choosePlottingProfile <- function(type) {  
  resetFrames()  
  comboMenu <- comboMenuProfiles(tt)
  tkgrid(tklabel(tt, text="Profil Nummer: "), comboMenu)
  tkbind(comboMenu, "<<ComboboxSelected>>", function() type(p[[as.integer(tclvalue(number))]]))
}

comboMenuProfiles <- function(widget) {
  items <- c() 
  for(x in p) {
    items <- c(items, x@number)
  }    
  assign("number", tclVar( ), envir=.GlobalEnv)
  return(ttkcombobox(widget, values=items, textvariable=number, width=3))  
}

heightAdjustmentGui <- function() {
  h <- tktoplevel(tt)
  tktitle(h) <- "Height Adjustment"
  
  comboMenu <- comboMenuProfiles(h)
  
  height <- tclVar(10)
  entry <- tkentry(h, width="10", textvariable = height)
  tkgrid(tklabel(h, text="Profil Nummer: "),
         comboMenu, entry, 
         tklabel(h, text=" Meter"))
  
  onOK <- function() {
    tkdestroy(h)
    tkfocus(tt)  
    heightAdjustment(p[[as.integer(tclvalue(number))]], as.integer(tclvalue(height)))
  }
  
  tkgrid(tkbutton(h, text="OK", command=onOK))
}

heightAdjustment <- function(Profile, height) {
  Profile@xyzData@heightAdaption$V2 <- 
    Profile@xyzData@heightAdaption$V2 + height
  p[Profile@number] <- Profile
  assign("p", p, envir = .GlobalEnv)
}

###---Settings---####
pointsize <- 10
colors <- c("blue", "green", "yellow", "orange", "red", "purple")

# Funktion zur Farb-Wertzuweisung
myColorRamp <- function(colors, values) { 
  #all_values <- c(min(p[[1]]@xyzData@heightAdaption$V3), max(p[[1]]@xyzData@heightAdaption$V3))
  v <- (values - min(values))/diff(range(values))
  #v <- (values - min(values))/diff(range(c(log(0.01),log(12000))))
  #v <- (values - min(log(p[[1]]@xyzData@heightAdaption$V3)))/diff(range(values))
  #v <- (values - min(values))/diff(range(log(p[[2]]@xyzData@heightAdaption$V3)))
  x <- colorRamp(colors)(v) 
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255) 
} 

settings <- function() {
  s <- tktoplevel(tt)
  tktitle(s) <- "GE3D Settings"
  
  SliderValue <- tclVar(pointsize)
  slider <- tkscale(s, from=0, to=20,
                    showvalue=T, variable=SliderValue,
                    resolution=1, orient="horizontal")
  tkgrid(tklabel(s, text="Pointsize: "), slider)
  
  colorType <- c("grey", "multicolored")
  
  color <- tclVar("grey")
  colorMenu <- ttkcombobox(s, values=colorType, textvariable=color, width=10)
  tkgrid(tklabel(s, text="Color: "), colorMenu)
  
  onOK <- function() {
    tkgrab.release(s)
    tkdestroy(s)
    tkfocus(tt)   
    assign("pointsize", tclvalue(SliderValue), envir=.GlobalEnv)
    switch (tclvalue(color),
            "grey" = {ramp <- c("black", "grey", "white")},
            "multicolored" = {ramp <- c("blue", "green", "yellow", "orange", "red", "purple")})
    assign("colors", ramp, envir=.GlobalEnv)
    
    # save settings to file
    write.table(c("Pointsize" = pointsize, "Color" = as.vector(colors)), file="ge3dSettings", quote=FALSE, col.names=FALSE)
  }
  
  tkgrid(tkbutton(s, text="OK", command=onOK))
}

###---GUI methods---####
info <- function() {
  tkmessageBox(title = "About GE3D",
               message = "Developed by Anja Kleebaum \n
Thanks to Prof. Dr. Klaus Bitzer & Richard Regner", 
               icon = "info", type = "ok")
}

# Save Files
saveFile <- function(type) { 
  fileType = "{{GE3D Project (R Workspace)} {.RData}} {{All files} *}"  
  multiFiles <- FALSE
  save <- function(fileName) {
    save.image(file=fileName, ascii=TRUE)
  }
  
  fileName <- as.character(tkgetSaveFile(filetypes = fileType))  
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
  } else {
    save(fileName)
  }
}

# Open Files
openFile <- function(type) {  
  switch(type,
         "xyz" = {
           fileType = "{{XYZ Files} {.xyz}} {{All files} *}"  
           multiFiles <- TRUE
           open <- function(fileName) {}
         },
         "raw" = {
           fileType = "{{Rawdata} {.dat}} {{Rawdata} {.txt}} {{All files} *}"  
           multiFiles <- TRUE
           open <- function(fileName) {}
         },
         "rdata" = {
           fileType = "{{GE3D Project (R Workspace)} {.RData}} {{All files} *}"  
           multiFiles <- FALSE
           open <- function(fileName) {
             load(fileName, envir = .GlobalEnv)
           }})
  
  fileName <- as.character(tkgetOpenFile(filetypes = fileType, multiple=FALSE))  
  
  if (!nchar(fileName)) {
    tkmessageBox(message = "No file was selected!")
  } else {
    open(fileName)
    return(fileName)
  } 
}

# Organize Project
organizeProject <- function() {
  or <- tktoplevel(tt)
  tktitle(or) <- "Organize current GE3D Project"
  tkwm.minsize(or, 700, 200)
  comboMenu <- comboMenuProfiles(or)

  tkbind(comboMenu, "<<ComboboxSelected>>", 
         function() {
           currentProfileNumber <- as.integer(tclvalue(number))           
           try(tkdestroy(profileFrame))           
           assign("profileFrame", tkframe(or, relief="groove", borderwidth=0), envir = .GlobalEnv)
           tkgrid(profileFrame)
           
           xyzLabel <- tklabel(profileFrame, text="xyz-File:")
           xyzAddress <- tclVar(p[[currentProfileNumber]]@xyzData@address)
           xyzEntry <- tkentry(profileFrame, width="60", textvariable = xyzAddress, state = "readonly")
           xyzButton <- tkbutton(profileFrame, text="Change", command = function() {
             tclvalue(xyzAddress) <- openFile("xyz")
             p[currentProfileNumber] <- setXyzAddress(p[[currentProfileNumber]], tclvalue(xyzAddress))
             assign("p", p, envir = .GlobalEnv)
           })
           tkgrid(xyzLabel, xyzEntry, xyzButton)
           
           rawLabel <- tklabel(profileFrame, text="raw-data-File:")
           rawAddress <- tclVar(p[[currentProfileNumber]]@rawData@address)
           rawEntry <- tkentry(profileFrame, width="60", textvariable = rawAddress, state = "readonly")
           rawButton <- tkbutton(profileFrame, text="Change", command = function() {
             tclvalue(rawAddress) <- openFile("raw")
             p[currentProfileNumber] <- setRawAddress(p[[currentProfileNumber]], tclvalue(rawAddress))
             assign("p", p, envir = .GlobalEnv)
           })
           tkgrid(rawLabel, rawEntry, rawButton)
           
           gpsLabel <- tklabel(profileFrame, text="gps-File:")
           gpsAddress <- tclVar(p[[currentProfileNumber]]@gpsCoordinates@address)
           gpsEntry <- tkentry(profileFrame, width="60", textvariable = gpsAddress, state = "readonly")
           gpsButton <- tkbutton(profileFrame, text="Change", command = function() {
             tclvalue(gpsAddress) <- openFile("raw")
             p[currentProfileNumber] <- setGpsAddress(p[[currentProfileNumber]], tclvalue(gpsAddress))
             assign("p", p, envir = .GlobalEnv)
           })
           tkgrid(gpsLabel, gpsEntry, gpsButton)
         })
  
  onOK <- function() {
    tkgrab.release(or)
    tkdestroy(or)
    tkfocus(tt)
    assign("p", p, envir = .GlobalEnv)
  }  
  
  ok.but <- tkbutton(or, text="OK", command = onOK)
  tkgrid(comboMenu, ok.but, sticky="w")
  
  return(p)
}

modalDialog <- function(title, question, entryInit, entryWidth = 20,
                        returnValOnCancel = "ID_CANCEL") {
  dlg <- tktoplevel()
  tkwm.deiconify(dlg)
  tkgrab.set(dlg)
  tkfocus(dlg)
  tkwm.title(dlg, title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                             textvariable = textEntryVarTcl)
  tkgrid(tklabel(dlg, text = "       "))
  tkgrid(tklabel(dlg, text = question), textEntryWidget)
  tkgrid(tklabel(dlg, text = "       "))
  ReturnVal <- returnValOnCancel
  
  onOK <- function() {
    ReturnVal <<- tclvalue(textEntryVarTcl)
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  onCancel <- function() {
    ReturnVal <<- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
    tkfocus(tt)
  }
  OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
  Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
  tkgrid(OK.but, Cancel.but)
  tkgrid(tklabel(dlg, text = "    "))
  
  tkfocus(dlg)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(tt)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkwait.window(dlg)
  
  return(ReturnVal)
}

dropCurrentProject <- function() {
  ReturnVal <- tkmessageBox(message = "Do you want to save before dropping current project?",
                            icon = "question", type = "yesnocancel", default = "yes")
  if(tclvalue(ReturnVal) == "cancel") {
    return()
  }
  else if(tclvalue(ReturnVal) == "yes") {
    saveFile("rdata")
  }
}

newProject <- function() {
  ReturnVal <- tkmessageBox(message = "Do you want to save before dropping current project?",
                            icon = "question", type = "yesnocancel", default = "yes")
  if(tclvalue(ReturnVal) == "cancel") {
    return()
  }
  else if(tclvalue(ReturnVal) == "yes") {
    saveFile("rdata")
  }
  
  try(rm(p, envir = .GlobalEnv))
  
  ReturnVal <- modalDialog("Profile Number", "Insert Profile Number", 1, 5)
  profiles <- list()
  for(i in 1:as.numeric(ReturnVal)) {
    profiles[i] <- new("Profile",
                       number = i)
  }
  return(profiles)
}

###---GUI Layout---####
### menubar
topMenu <- tkmenu(tt)
tkconfigure(tt, menu = topMenu)
fileMenu <- tkmenu(topMenu, tearoff = FALSE)
editMenu <- tkmenu(topMenu, tearoff = FALSE)
plot2dMenu <- tkmenu(topMenu, tearoff = FALSE)
plot3dMenu <- tkmenu(topMenu, tearoff = FALSE)
helpMenu <- tkmenu(topMenu, tearoff = FALSE)

# cascaded menus
openFileMenu <- tkmenu(topMenu, tearoff = FALSE)
saveFileMenu <- tkmenu(topMenu, tearoff = FALSE) 
plot2dRaw <- tkmenu(topMenu, tearoff = FALSE)
plot2dXyz <- tkmenu(topMenu, tearoff = FALSE)
plot3dSingle <- tkmenu(topMenu, tearoff = FALSE) 
plot3dFew <- tkmenu(topMenu, tearoff = FALSE)
plot3dAll <- tkmenu(topMenu, tearoff = FALSE) 
example <- tkmenu(topMenu, tearoff = FALSE)

tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
tkadd(topMenu, "cascade", label = "Edit", menu = editMenu)
tkadd(topMenu, "cascade", label = "Plot 2D", menu = plot2dMenu)
tkadd(topMenu, "cascade", label = "Plot 3D", menu = plot3dMenu)
tkadd(topMenu, "cascade", label = "Help", menu = helpMenu)

tkadd(fileMenu, "command", label = "New Project", command = function() {
  assign("p", newProject(), envir = .GlobalEnv)
  organizeProject()})
tkadd(fileMenu, "command", label = "Open Project", command = function() openFile("rdata"))
tkadd(fileMenu, "command", label = "Save Project", command = function() saveFile("rdata"))
tkadd(fileMenu, "command", label = "Organize Project", command = function() organizeProject())
tkadd(fileMenu, "separator")
tkadd(fileMenu, "command", label = "Quit Program", command = function() {
  ReturnVal <- tkmessageBox(message = "Do you want to save before quitting?",
               icon = "question", type = "yesnocancel", default = "yes")
  if(tclvalue(ReturnVal) == "cancel") {
    return()
  }
  else if(tclvalue(ReturnVal) == "yes") {
    saveFile("rdata")
  }
  tkdestroy(tt)
  })

tkadd(editMenu, "command", label = "Settings", command = function() settings())
tkadd(editMenu, "separator")
tkadd(editMenu, "command", label = "Height Adjustment", command = function() {
  heightAdjustmentGui()
})

tkadd(plot2dMenu, "cascade", label = "Rawdata", menu = plot2dRaw)
tkadd(plot2dMenu, "cascade", label = "After Inversion", menu = plot2dXyz)

tkadd(plot3dMenu, "command", label = "Single Profiles", command = function() choosePlottingProfile(plot3dXyz))
tkadd(plot3dMenu, "command", label = "All Profiles", command = function() { 
  for(x in p) {    
    plot3dXyz(p[[x@number]])
  }})

tkadd(helpMenu, "cascade", label = "Load Example", menu = example)
tkadd(helpMenu, "separator")
tkadd(helpMenu, "command", label = "About Program", command = info)

tkadd(plot2dRaw, "command", label = "Points", command = function() choosePlottingProfile(plotRaw))
tkadd(plot2dRaw, "command", label = "Levels", command = function() choosePlottingProfile(levelplotRaw))

tkadd(plot2dXyz, "command", label = "Points", command = function() choosePlottingProfile(plotXyz))
tkadd(plot2dXyz, "command", label = "Levels", command = function() choosePlottingProfile(levelplotXyz))
tkadd(plot2dXyz, "command", label = "Points with Topography", command = function() choosePlottingProfile(plotXyzHeight))
tkadd(plot2dXyz, "command", label = "Levels with Topography", command = function() choosePlottingProfile(levelplotXyzHeight))

tkadd(example, "command", label = "Sinkhole", command = function() source("example/example.r"))

### frames
deleteFrame <- function(frameName) {
  tkdestroy(frameName)
}

newFrame <- function(frameName) {
  assign(frameName, tkframe(tt, relief="groove", borderwidth=0), envir = .GlobalEnv)  
}

resetFrames <- function() {
  deleteFrame(frameLeft)
  deleteFrame(frameRight)
  newFrame("frameLeft")
  newFrame("frameRight")
  return(tkgrid(frameLeft, frameRight))
}

# left frame
newFrame("frameLeft")
tkgrid(tklabel(frameLeft, 
               text=""))

# right frame
newFrame("frameRight")
tkgrid(tklabel(frameRight, text="Welcome to the Software for 3D-Presentation of 2D-Geoelectric Profiles"))

# create frames
tkgrid(frameLeft, frameRight)

# don't terminate 
Sys.sleep(1e30)