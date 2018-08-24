# geoelectrics
R package to visualize geoelectric resistivity measurement profiles.

[![Build Status](https://travis-ci.org/kleebaum/geoelectrics.svg?branch=master)](https://travis-ci.org/kleebaum/geoelectrics)

![example plot](https://raw.githubusercontent.com/kleebaum/geoelectrics/master/inst/extdata/sinkhole.png)

Electrical resistivity tomography is an efficient geophysical technique to investigate the spatial extent of subsurface structures. Many scientific objectives in geology demand three-dimensional imaging. 
3D electrical resistivity tomography provides a technique to survey three-dimensional structures. 
Nonetheless, 3D electrical resistivity tomography requires an enormous amount of time as well as a high work load. 
In most cases, 2D electrical resistivity tomography is used to obtain two-dimensional subsurface profiles. 
This R package enables the user to visualize two-dimensional profiles in three dimensions.
 
## Installation Prerequisites
- R: http://www.r-project.org/
- the following packages from CRAN:
  - rgl
  - lattice
  - fields
  
## Example Usage
### Profile
An object of the Profile class is created for each profile:
```
p1 <- new(
  "Profile",
  title = "Profile 1",
  xyzData =
    new("XyzData",
        address = "./inst/extdata/processed/p1_DipolDipol_SW-NE.xyz"),
  rawData =
    new("RawData",
        address = "./inst/extdata/raw/p1_DipolDipol_SW-NE.dat"),
  measurementType = "DipolDipol",
  gpsCoordinates =
    new("GpsCoordinates",
        address = "./inst/extdata/gps/p1.txt")
)
```

### Profile Set
An instance of the ProfileSet class is created using a list of single profiles.
```
sinkhole <- new("ProfileSet",
                profiles = list(p1, p2, p3),
                title="Sinkhole")
```

### Adjust Profile Height
GPS measurement heights might differ. Therefore, the height of a single profile can be adjusted.
```
p3 <- heightAdjustment(p3, -10)
```

### Plotting Methods
- ```plotRaw()```
- ```plotRawHeight()```
- ```plotXyz()```
- ```plotXyzHeight()```
- ```levelplotRaw()```
- ```levelplotXyz()```
- ```levelplotXyzHeight()```
- ```levelplotLegendLabel()```
- ```plotLegend()```
- ```plotIntersect()```
- ```plot3dXyz()```

## Graphical User Interface
This R package provides a graphical user interface (GUI). 
The following packages from CRAN are needed for the GUI:
  - tcltk
  - tkrplot
Please perform ONE of the following steps to start the GUI:
  - open R in a terminal and type ```source(system.file("gui/gui.r", package="geoelectrics"))```
  - navigate into the *gui* folder and execute *start_gui.sh* (Unix) or *start_gui.bat* (Windows, make sure that R is added to the PATH variable)