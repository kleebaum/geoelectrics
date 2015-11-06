# 3D-Presentation of Geoelectric Profiles Version 1.1
# Graphical User Interface
# Anja Kleebaum

source("kleelectrics.r")

### load packages
require(tcltk)
library(tkrplot)

tt <- tktoplevel()
tkwm.minsize(tt, 400, 300)
tktitle(tt) <- "Kleelectrics: 3D-Presentation of Geoelectric Profiles"

###---Plotting functions in GUI---###
plotXyzGui <- function(Profile) {
  try(tkgrid.remove(img))
  img <<- tkrplot(frameRight, function() { plotXyz(Profile) })
  tkgrid(img)
}

levelplotXyzGui <- function(Profile) {
  img <<- levelplotXyz(Profile)
}

plotXyzHeightGui <- function(Profile) {
  try(tkgrid.remove(img))
  img <<- tkrplot(frameRight, function() { plotXyzHeight(Profile) })
  tkgrid(img)
}

levelplotXyzHeightGui <- function(Profile) {
  img <<- levelplotXyzHeight(Profile)
  #source("savePlots.r", echo=T)
}

levelplotRawGui <- function(Profile) {
  img <<- levelplotRaw(Profile)
  #source("savePlots.r", echo=T)
}

plotRawGui <- function(Profile) {  
  try(tkgrid.remove(img))
  img <<- tkrplot(frameRight, function() { plotRaw(Profile) })  
  tkgrid(img)  
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
  number <<- tclVar()
  #assign("number", tclVar(), .GlobalEnv)
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

###---GUI methods---####
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
    pointsize <<- tclvalue(SliderValue)
    switch (tclvalue(color),
            "grey" = {ramp <- c("black", "grey", "white")},
            "multicolored" = {ramp <- c("blue", "green", "yellow", "orange", "red", "purple")})
    colors <<- ramp
    
    # save settings to file
    write.table(c("Pointsize" = pointsize, "Color" = as.vector(colors)), file="ge3dSettings", quote=FALSE, col.names=FALSE)
  }
  
  tkgrid(tkbutton(s, text="OK", command=onOK))
}

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

tkadd(plot2dRaw, "command", label = "Points", command = function() choosePlottingProfile(plotRawGui))
tkadd(plot2dRaw, "command", label = "Levels", command = function() choosePlottingProfile(levelplotRawGui))

tkadd(plot2dXyz, "command", label = "Points", command = function() choosePlottingProfile(plotXyzGui))
tkadd(plot2dXyz, "command", label = "Levels", command = function() choosePlottingProfile(levelplotXyzGui))
tkadd(plot2dXyz, "command", label = "Points with Topography", command = function() choosePlottingProfile(plotXyzHeightGui))
tkadd(plot2dXyz, "command", label = "Levels with Topography", command = function() choosePlottingProfile(levelplotXyzHeightGui))

tkadd(example, "command", label = "Sinkhole", command = function() source("example/example.r"))

### frames
deleteFrame <- function(frameName) {
  tkdestroy(frameName)
}

newFrame <- function(frameName) {
  frameName <<- tkframe(tt, relief="groove", borderwidth=0)
}

resetFrames <- function() {
  #deleteFrame(frameLeft)
  #deleteFrame(frameRight)
  frameLeft <<- newFrame("frameLeft")
  frameRight <<- newFrame("frameRight")
  return(tkgrid(frameLeft, frameRight))
}

# left frame
frameLeft <<- newFrame("frameLeft")
tkgrid(tklabel(frameLeft, 
               text="test"))

# right frame
frameRight <<- newFrame("frameRight")
tkgrid(tklabel(frameRight, text="Welcome to the Software for 3D-Presentation of 2D-Geoelectric Profiles"))

# create frames
tkgrid(frameLeft, frameRight)

# don't terminate 
Sys.sleep(1e30)