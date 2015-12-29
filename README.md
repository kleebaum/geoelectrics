# geoelectrics
R package to plot geoelectric resistivity measurement profiles.

![example plot](https://github.com/kleebaum/geoelectrics/blob/master/example/sinkhole.png)

Electrical resistivity tomography is an efficient geophysical technique to investigate the spatial extent of subsurface structures. Many scientific objectives in geology demand three-dimensional imaging. 

3D electrical resistivity tomography provides a technique to survey three-dimensional structures. Nonetheless, 3D electrical resistivity tomography requires an enormous amount of time as well as high work load. 

In most cases 2D electrical resistivity tomography is used to obtain two-dimensional subsurface profiles. This package enables to visualize two-dimensional profiles in three dimensions.
 
## Requirements
- installation of R: http://www.r-project.org/
- the following packages from CRAN:
  - rgl
  - lattice
  - fields
  
## Example Usage
### Profile
An object of the Profile class is created for each profile:
```
p1 <- new("Profile",
            title = "Profile 1",
            xyzData = 
              new("XyzData", 
                  address = "example/xyzFiles/p1_DipolDipol_SW-NE.xyz"),
            rawData = 
              new("RawData",
                  address = "example/rawdata/p1_DipolDipol_SW-NE.dat"),
            measurementType = "DipolDipol",
            gpsCoordinates = 
              new("GpsCoordinates",
                  address = "example/gps/p1.txt"))
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