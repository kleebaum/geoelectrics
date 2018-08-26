# geoelectrics
R package to visualize geoelectric resistivity measurement profiles.

[![Build Status](https://travis-ci.org/kleebaum/geoelectrics.svg?branch=master)](https://travis-ci.org/kleebaum/geoelectrics)
[![Coverage Status](https://codecov.io/gh/kleebaum/geoelectrics/branch/master/graph/badge.svg)](https://codecov.io/gh/kleebaum/geoelectrics)
[![CRAN Version](http://www.r-pkg.org/badges/version/geoelectrics)](https://cran.r-project.org/package=geoelectrics)


Electrical resistivity tomography is an efficient geophysical technique to investigate the spatial extent of subsurface structures. Many scientific objectives in geology demand three-dimensional imaging. 
3D electrical resistivity tomography provides a technique to survey three-dimensional structures. 
Nonetheless, 3D electrical resistivity tomography requires an enormous amount of time as well as a high work load. 
In most cases, 2D electrical resistivity tomography is used to obtain two-dimensional subsurface profiles. 
This R package enables the user to visualize two-dimensional profiles in three dimensions.
 
## Installation 

### Prerequisites
The following prerequisites are necessary for the *geoelectrics* R package:
- the [R software environment](http://www.r-project.org/)
- the following packages from the [Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/):
  - [rgl](https://cran.r-project.org/package=rgl/)
  - [lattice](https://cran.r-project.org/package=lattice/)
  - [fields](https://cran.r-project.org/package=fields/)
- optionally: an integrated development environment such as [RStudio](https://www.rstudio.com/)
  
### Build from Source Code
You need to install the *devtools* package to build the *geoelectrics* package from source code:
```
install.packages("devtools")
library(devtools)
```

Perform ONE of the following steps to build the *geoelectrics* package:
- Install the package from GitHub via ```install_github('kleebaum/geoelectrics')```
- Clone the source code, navigate to the *geoelectrics* folder, open R in a terminal/RStudio and type ```build()```

### Installation from CRAN
[The *geoelectrics* package is available via the Comprehensive R Archive Network (CRAN).](https://cran.r-project.org/package=geoelectrics/)
```
install.packages("geoelectrics")
library(geoelectrics)
```

## Domain Model
The domain model shows the fundamental classes involved in electrical resistivity tomography.
A 2D subsurface profile comprises GPS coordinates and raw data.
The raw data is collected using a certain type of measurement (e.g., Dipole Dipole or Wenner). 
The raw data needs to be processed resulting in processed data.
A profile set comprises 2D profiles.

![Domain model as a UML class diagram](https://raw.githubusercontent.com/kleebaum/geoelectrics/master/inst/img/domain_data.png)
*Domain model for electrical resistivity tomography*

## Implementation Details
The *geoelectrics* R package provides five model classes to represent geolectric resistivity measurement data:
- The *Profile* class represents a 2D geolectric resistivity measurement profile.
- The *ProfileSet* class represents a set of 2D geolectric profiles in order to visualize them in three dimensions.
- The *RawData* class represents geolectric raw data.
- The *GpsCoordinates* class represents the GPS coordinates of a single profile.
- The *XyzData* class represents processed (inverted) geolectric data.

![UML class diagram](https://raw.githubusercontent.com/kleebaum/geoelectrics/master/inst/img/class_diagram.png)
*Class diagram of the geoelectrics R package*

## Example Usage
Run ```demo(geoelectrics)``` to get an impression of the *geoelectrics* R package.

![Example plot created with the geoelectrics R package](https://raw.githubusercontent.com/kleebaum/geoelectrics/master/inst/img/sinkhole.png)

*Example plot created with the geoelectrics R package.*

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
![GUI](https://raw.githubusercontent.com/kleebaum/geoelectrics/master/inst/img/gui.png)

The following packages from CRAN are needed for the GUI:
  - tcltk
  - tkrplot

Perform ONE of the following steps to start the GUI:
  - open R in a terminal and type ```source(system.file("gui/gui.r", package="geoelectrics"))```
  - navigate into the *gui* folder and execute *start_gui.sh* (Unix) or *start_gui.bat* (Windows, make sure that R is added to the PATH variable)