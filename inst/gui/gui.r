# 3D-Visualization of Geoelectric Resistivity Measurement Profiles
# Graphical User Interface
# Author: Anja Kleebaum

### load packages
library(geoelectrics)
require(tcltk)
library(tkrplot)

### looking for working directory and change working directory to location of this file
try(setwd(dirname(as.character(sys.calls()[[1]][2]))))

topLevelWindow <- tktoplevel()
tkwm.minsize(topLevelWindow, 400, 300)
tktitle(topLevelWindow) <-
  '3D-Visualization of Geoelectric Resistivity Measurement Profiles'

### Load example when starting
data(sinkhole)
profiles <-
  list(sinkhole@profiles[[1]], sinkhole@profiles[[2]], sinkhole@profiles[[3]])
selectedProfileNumber <- tclVar(1)

###---Plotting functions in GUI---###
plotProcessedDataGui <- function(profile) {
  try(tkgrid.remove(img))
  img <- tkrplot(frameRight, function() {
    plot(
      profile,
      dataType = 'processed',
      withTopo = FALSE,
      main = profile@title,
      ylab = 'Depth [m]'
    )
  })
  assign("img", img, envir = .GlobalEnv)
  tkgrid(img)
}

levelplotProcessedDataGui <- function(profile) {
  img <- levelplot(
    profile,
    dataType = 'processed',
    withTopo = FALSE,
    main = profile@title
  )
  assign("img", img, envir = .GlobalEnv)
  print(img)
}

plotProcessedDataWithTopoGui <- function(profile) {
  try(tkgrid.remove(img))
  img <- tkrplot(frameRight, function() {
    plot(profile, dataType = 'processed', withTopo = TRUE)
  })
  assign("img", img, envir = .GlobalEnv)
  tkgrid(img)
}

levelplotProcessedDataWithTopoGui <- function(profile) {
  img <- levelplot(profile, dataType = 'processed', withTopo = TRUE)
  assign("img", img, envir = .GlobalEnv)
  print(img)
}

levelplotRawDataGui <- function(profile) {
  img <- levelplot(profile, dataType = 'raw', withTopo = FALSE)
  assign("img", img, envir = .GlobalEnv)
  print(img)
}

plotRawDataGui <- function(profile) {
  try(tkgrid.remove(img))
  img <- tkrplot(frameRight, function() {
    plot(
      profile,
      dataType = 'raw',
      withTopo = FALSE,
      main = profile@title,
      ylab = 'Depth [m]'
    )
  })
  assign("img", img, envir = .GlobalEnv)
  tkgrid(img)
}

createComboBoxForProfiles <- function(widget, onChangedSelection) {
  items <- c()
  for (profile in profiles) {
    items <- c(items, profile@number)
  }
  selectedProfileNumber <<- tclVar(1)
  
  try(tkdestroy(comboBox))
  comboBox <- ttkcombobox(widget,
                          values = items,
                          textvariable = selectedProfileNumber,
                          width = 3)
  assign('comboBox', comboBox, envir = .GlobalEnv)
  tkbind(comboBox, '<<ComboboxSelected>>', function() {
    onChangedSelection(as.integer(tclvalue(selectedProfileNumber)))
  })
  
  try(tkdestroy(comboBoxLabel))
  comboBoxLabel <- tklabel(widget, text = 'Profile Number: ')
  assign('comboBoxLabel', comboBoxLabel, envir = .GlobalEnv)
  
  tkgrid(comboBoxLabel, sticky = 'w')
  tkgrid(comboBox, sticky = 'w', pady = c(0, 10))
  
  onChangedSelection(1)
}

adjustHeightGui <- function() {
  adjustHeightDialog <- tktoplevel(topLevelWindow)
  tktitle(adjustHeightDialog) <- 'Height Adjustment'
  
  onChangedSelection <- function(selectedProfileNumber) {
    
  }
  createComboBoxForProfiles(adjustHeightDialog, onChangedSelection)
  
  height <- tclVar(10)
  entry <-
    tkentry(adjustHeightDialog,
            width = '10',
            textvariable = height)
  tkgrid(entry, tklabel(adjustHeightDialog, text = ' Meter'))
  
  onOK <- function() {
    tkdestroy(adjustHeightDialog)
    tkfocus(topLevelWindow)
    number <- as.integer(tclvalue(selectedProfileNumber))
    profiles[[number]] <<-
      adjustHeight(profiles[[number]], as.integer(tclvalue(height)))
  }
  
  tkgrid(tkbutton(adjustHeightDialog, text = 'OK', command = onOK))
}

###---GUI methods---####
settings <- function() {
  if (file.exists('config')) {
    config <- read.table('config')
    pointsize <- as.character(config[1, 2])
    colorRamp <- as.character(config[2, 2])
  } else {
    pointsize <- 10
    colorRamp <- 'grey'
  }
  
  settingsDialog <- tktoplevel(topLevelWindow)
  tktitle(settingsDialog) <- 'Settings'
  
  sliderValue <- tclVar(pointsize)
  slider <- tkscale(
    settingsDialog,
    from = 0,
    to = 20,
    showvalue = T,
    variable = sliderValue,
    resolution = 1,
    orient = 'horizontal'
  )
  tkgrid(tklabel(settingsDialog, text = 'Pointsize: '), slider)
  
  colorRamps <- c('grey', 'multicolored')
  
  colorRamp <- tclVar(colorRamp)
  colorComboBox <-
    ttkcombobox(
      settingsDialog,
      values = colorRamps,
      textvariable = colorRamp,
      width = 10
    )
  tkgrid(tklabel(settingsDialog, text = 'Color: '), colorComboBox)
  
  onOK <- function() {
    tkgrab.release(settingsDialog)
    tkdestroy(settingsDialog)
    tkfocus(topLevelWindow)
    pointsize <- tclvalue(sliderValue)
    assign("pointsize", pointsize, envir = .GlobalEnv)
    
    switch(tclvalue(colorRamp),
           'grey' = {
             colors <- c('white', 'grey', 'black')
           },
           'multicolored' = {
             colors <- c('blue', 'green', 'yellow', 'orange', 'red', 'purple')
           })
    assign("colors", colors, envir = .GlobalEnv)
    
    # save settings to file
    write.table(
      c(
        'Pointsize' = pointsize,
        'ColorRamp' = tclvalue(colorRamp)
      ),
      file = 'config',
      quote = FALSE,
      col.names = FALSE
    )
  }
  
  tkgrid(tkbutton(settingsDialog, text = 'OK', command = onOK))
}

info <- function() {
  tkmessageBox(
    title = 'About this program',
    message = ' Developed by Anja Kleebaum\n Thanks to Prof. Dr. Klaus Bitzer & Richard Regner',
    icon = 'info',
    type = 'ok'
  )
}

# Save File
saveFile <- function(type) {
  fileType = '{{Geoelectrics Project (R Workspace)} {.RData}} {{All files} *}'
  save <- function(fileName) {
    save.image(file = fileName, ascii = TRUE)
  }
  
  fileName <- as.character(tkgetSaveFile(filetypes = fileType))
  
  if (!nchar(fileName)) {
    tkmessageBox(message = 'No file was selected.')
  } else {
    save(fileName)
  }
}

# Open Files
openFile <- function(type) {
  switch(type,
         'processed' = {
           fileType = '{{XYZ Files} {.xyz}} {{All files} *}'
           multiFiles <- TRUE
           open <- function(fileName) {
             
           }
         },
         'raw' = {
           fileType = '{{Rawdata} {.dat}} {{Rawdata} {.txt}} {{All files} *}'
           multiFiles <- TRUE
           open <- function(fileName) {
             
           }
         },
         'rdata' = {
           fileType = '{{Geoelectrics Project (R Workspace)} {.RData}} {{All files} *}'
           multiFiles <- FALSE
           open <- function(fileName) {
             load(fileName, envir = .GlobalEnv)
           }
         })
  
  fileName <-
    as.character(tkgetOpenFile(filetypes = fileType, multiple = FALSE))
  
  if (!nchar(fileName)) {
    tkmessageBox(message = 'No file was selected!')
  } else {
    open(fileName)
    return(fileName)
  }
}

# Update Project
updateProject <- function() {
  updateProjectDialog <- tktoplevel(topLevelWindow)
  tktitle(updateProjectDialog) <-
    'Update geoelectrics project settings'
  tkwm.minsize(updateProjectDialog, 700, 200)
  
  onOK <- function() {
    tkgrab.release(updateProjectDialog)
    tkdestroy(updateProjectDialog)
    tkfocus(topLevelWindow)
    assign('profiles', profiles, envir = .GlobalEnv)
  }
  
  onChangedSelection <- function(selectedProfileNumber) {
    try(tkdestroy(profileFrame))
    profileFrame <- tkframe(updateProjectDialog,
                            relief = 'groove',
                            borderwidth = 0)
    assign('profileFrame', profileFrame, envir = .GlobalEnv)
    tkgrid(profileFrame)
    
    processedDataLabel <-
      tklabel(profileFrame, text = 'Processed data file:')
    processedDataAddress <-
      tclVar(p[[selectedProfileNumber]]@processedData@address)
    processedDataEntry <-
      tkentry(
        profileFrame,
        width = '60',
        textvariable = processedDataAddress,
        state = 'readonly'
      )
    processedDataButton <-
      tkbutton(
        profileFrame,
        text = 'Change',
        command = function() {
          tclvalue(processedDataAddress) <- openFile('processed')
          profiles[[selectedProfileNumber]]@processedData <-
            new('ProcessedData', tclvalue(processedDataAddress))
          assign('profiles', profiles, envir = .GlobalEnv)
        }
      )
    tkgrid(processedDataLabel,
           processedDataEntry,
           processedDataButton)
    
    rawLabel <-
      tklabel(profileFrame, text = 'Raw data file:')
    rawAddress <-
      tclVar(profiles[[selectedProfileNumber]]@rawData@address)
    rawEntry <-
      tkentry(
        profileFrame,
        width = '60',
        textvariable = rawAddress,
        state = 'readonly'
      )
    rawButton <-
      tkbutton(
        profileFrame,
        text = 'Change',
        command = function() {
          tclvalue(rawAddress) <- openFile('raw')
          profiles[[selectedProfileNumber]]@rawData <-
            new('RawData', tclvalue(rawAddress))
          assign('profiles', profiles, envir = .GlobalEnv)
        }
      )
    tkgrid(rawLabel, rawEntry, rawButton)
    
    gpsLabel <- tklabel(profileFrame, text = 'GPS file:')
    gpsAddress <-
      tclVar(profiles[[selectedProfileNumber]]@gpsCoordinates@address)
    gpsEntry <-
      tkentry(
        profileFrame,
        width = '60',
        textvariable = gpsAddress,
        state = 'readonly'
      )
    gpsButton <-
      tkbutton(
        profileFrame,
        text = 'Change',
        command = function() {
          tclvalue(gpsAddress) <- openFile('raw')
          profiles[[selectedProfileNumber]]@gpsCoordinates <-
            new('GpsCoordinates', tclvalue(gpsAddress))
          assign('profiles', profiles, envir = .GlobalEnv)
        }
      )
    tkgrid(gpsLabel, gpsEntry, gpsButton)
    
    okButton <- tkbutton(profileFrame, text = 'OK', command = onOK)
    tkgrid(
      okButton,
      sticky = 'e',
      columnspan = 3,
      pady = c(10, 10)
    )
  }
  
  createComboBoxForProfiles(updateProjectDialog, onChangedSelection)
  
  return(p)
}

modalDialog <- function(title,
                        question,
                        entryInit,
                        entryWidth = 20,
                        returnValOnCancel = 'ID_CANCEL') {
  dialog <- tktoplevel()
  tkwm.deiconify(dialog)
  tkgrab.set(dialog)
  tkfocus(dialog)
  tkwm.title(dialog, title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tkentry(dialog,
                             width = paste(entryWidth),
                             textvariable = textEntryVarTcl)
  tkgrid(tklabel(dialog, text = '       '))
  tkgrid(tklabel(dialog, text = question), textEntryWidget)
  tkgrid(tklabel(dialog, text = '       '))
  returnVal <- returnValOnCancel
  
  onOK <- function() {
    returnVal <<- tclvalue(textEntryVarTcl)
    tkgrab.release(dialog)
    tkdestroy(dialog)
    tkfocus(topLevelWindow)
  }
  onCancel <- function() {
    tkgrab.release(dialog)
    tkdestroy(dialog)
    tkfocus(topLevelWindow)
    return(returnValOnCancel)
  }
  okButton <- tkbutton(dialog, text = '   OK   ', command = onOK)
  cancelButton <-
    tkbutton(dialog, text = ' Cancel ', command = onCancel)
  tkgrid(okButton, cancelButton)
  tkgrid(tklabel(dialog, text = '    '))
  
  tkfocus(dialog)
  tkbind(dialog, '<Destroy>', function() {
    tkgrab.release(dialog)
    tkfocus(topLevelWindow)
  })
  tkbind(textEntryWidget, '<Return>', onOK)
  tkwait.window(dialog)
  
  return(returnVal)
}

dropCurrentProject <- function() {
  returnVal <-
    tkmessageBox(
      message = 'Do you want to save before dropping current project?',
      icon = 'question',
      type = 'yesnocancel',
      default = 'yes'
    )
  if (tclvalue(returnVal) == 'cancel') {
    return()
  }
  else if (tclvalue(returnVal) == 'yes') {
    saveFile('rdata')
  }
}

newProject <- function() {
  returnVal <-
    tkmessageBox(
      message = 'Do you want to save before dropping current project?',
      icon = 'question',
      type = 'yesnocancel',
      default = 'yes'
    )
  if (tclvalue(returnVal) == 'cancel') {
    return()
  }
  else if (tclvalue(returnVal) == 'yes') {
    saveFile('rdata')
  }
  
  try(rm(profiles, envir = .GlobalEnv))
  
  numberOfProfiles <-
    modalDialog('Create a new geoelctrics project',
                'Insert Number of Profiles',
                1,
                5)
  
  profiles <- list()
  for (i in 1:as.numeric(numberOfProfiles)) {
    profiles[i] <- new('Profile',
                       number = i)
  }
  assign('profiles', profiles, envir = .GlobalEnv)
  updateProject()
}

### menubar
topMenu <- tkmenu(topLevelWindow)
tkconfigure(topLevelWindow, menu = topMenu)
fileMenu <- tkmenu(topMenu, tearoff = FALSE)
editMenu <- tkmenu(topMenu, tearoff = FALSE)
plot2dMenu <- tkmenu(topMenu, tearoff = FALSE)
plot3dMenu <- tkmenu(topMenu, tearoff = FALSE)
helpMenu <- tkmenu(topMenu, tearoff = FALSE)

# cascaded menus
openFileMenu <- tkmenu(topMenu, tearoff = FALSE)
saveFileMenu <- tkmenu(topMenu, tearoff = FALSE)
plotRawDataMenu <- tkmenu(topMenu, tearoff = FALSE)
plotProcessedDataMenu <- tkmenu(topMenu, tearoff = FALSE)
plot3dSingle <- tkmenu(topMenu, tearoff = FALSE)
plot3dAll <- tkmenu(topMenu, tearoff = FALSE)
example <- tkmenu(topMenu, tearoff = FALSE)

tkadd(topMenu, 'cascade', label = 'File', menu = fileMenu)
tkadd(topMenu, 'cascade', label = 'Edit', menu = editMenu)
tkadd(topMenu, 'cascade', label = 'Plot 2D', menu = plot2dMenu)
tkadd(topMenu, 'cascade', label = 'Plot 3D', menu = plot3dMenu)
tkadd(topMenu, 'cascade', label = 'Help', menu = helpMenu)

tkadd(
  fileMenu,
  'command',
  label = 'New Project',
  command = function() {
    newProject()
  }
)
tkadd(
  fileMenu,
  'command',
  label = 'Open Project',
  command = function()
    openFile('rdata')
)
tkadd(
  fileMenu,
  'command',
  label = 'Save Project',
  command = function()
    saveFile('rdata')
)
tkadd(
  fileMenu,
  'command',
  label = 'Update Project',
  command = function()
    updateProject()
)
tkadd(fileMenu, 'separator')
tkadd(
  fileMenu,
  'command',
  label = 'Quit Program',
  command = function() {
    ReturnVal <-
      tkmessageBox(
        message = 'Do you want to save before quitting?',
        icon = 'question',
        type = 'yesnocancel',
        default = 'yes'
      )
    if (tclvalue(ReturnVal) == 'cancel') {
      return()
    }
    else if (tclvalue(ReturnVal) == 'yes') {
      saveFile('rdata')
    }
    tkdestroy(topLevelWindow)
  }
)

tkadd(
  editMenu,
  'command',
  label = 'Plotting Settings',
  command = function()
    settings()
)
tkadd(editMenu, 'separator')
tkadd(
  editMenu,
  'command',
  label = 'Height Adjustment',
  command = function() {
    adjustHeightGui()
  }
)

tkadd(plot2dMenu, 'cascade', label = 'Rawdata', menu = plotRawDataMenu)
tkadd(plot2dMenu, 'cascade', label = 'After Inversion', menu = plotProcessedDataMenu)

tkadd(
  plot3dMenu,
  'command',
  label = 'Single Profiles',
  command = function()
    plotProfileWithPlottingFunction(function(profile) {
      plot3d(profile, psize = pointsize, col = colors)
    })
)
tkadd(
  plot3dMenu,
  'command',
  label = 'All Profiles',
  command = function() {
    for (i in 1:length(profiles)) {
      plot3d(profiles[[i]], psize = pointsize, col = colors)
    }
  }
)

tkadd(helpMenu, 'cascade', label = 'Load Example', menu = example)
tkadd(helpMenu, 'separator')
tkadd(helpMenu, 'command', label = 'About Program', command = info)

plotProfileWithPlottingFunction <- function(plottingFunction) {
  resetFrames()
  onChangedSelection <- function(selectedProfileNumber) {
    plottingFunction(profiles[[selectedProfileNumber]])
  }
  createComboBoxForProfiles(topLevelWindow, onChangedSelection)
}

tkadd(
  plotRawDataMenu,
  'command',
  label = 'Points',
  command = function()
    plotProfileWithPlottingFunction(plotRawDataGui)
)
tkadd(
  plotRawDataMenu,
  'command',
  label = 'Levels',
  command = function()
    plotProfileWithPlottingFunction(levelplotRawDataGui)
)

tkadd(
  plotProcessedDataMenu,
  'command',
  label = 'Points',
  command = function()
    plotProfileWithPlottingFunction(plotProcessedDataGui)
)
tkadd(
  plotProcessedDataMenu,
  'command',
  label = 'Levels',
  command = function()
    plotProfileWithPlottingFunction(levelplotProcessedDataGui)
)
tkadd(
  plotProcessedDataMenu,
  'command',
  label = 'Points with Topography',
  command = function()
    plotProfileWithPlottingFunction(plotProcessedDataWithTopoGui)
)
tkadd(
  plotProcessedDataMenu,
  'command',
  label = 'Levels with Topography',
  command = function()
    plotProfileWithPlottingFunction(levelplotProcessedDataWithTopoGui)
)

tkadd(
  example,
  'command',
  label = 'Sinkhole',
  command = function() {
    data(sinkhole)
    profiles <-
      list(sinkhole@profiles[[1]], sinkhole@profiles[[2]], sinkhole@profiles[[3]])
    assign('profiles', profiles, envir = .GlobalEnv)
  }
)

### frames
resetFrames <- function() {
  deleteFrame(frameLeft)
  deleteFrame(frameRight)
  frameLeft <<- newFrame('frameLeft')
  frameRight <<- newFrame('frameRight')
  return(tkgrid(frameLeft, frameRight))
}

deleteFrame <- function(frameName) {
  tkdestroy(frameName)
}

newFrame <- function(frameName) {
  frameName <<-
    tkframe(topLevelWindow, relief = 'groove', borderwidth = 0)
}

# left frame
frameLeft <- newFrame('frameLeft')
tkgrid(tklabel(frameLeft, text = ''))

# right frame
frameRight <- newFrame('frameRight')
tkgrid(
  tklabel(frameRight, text = 'Welcome to the Software for 3D-Presentation of 2D-Geoelectric Profiles')
)

# create frames
tkgrid(frameLeft, frameRight)

# don't terminate
#Sys.sleep(360000)