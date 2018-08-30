# geoelectrics 0.2.1

## News
* The GUI is now available through the `geoelectricsGui()` function.

# geoelectrics 0.2.0

## News
* The package provides an example geoelectrics data set of a filled sinkhole and a demo script. The demo can be run with `demo(geolectrics)`.
* The package contains unit tests written with [testthat](https://github.com/r-lib/testthat). It currently has a line coverage of 99%! (Thanks to the [covr](https://github.com/r-lib/testthat) package.)
* The generic `plot` and `levelplot` functions were added.
* The `parseRawDataMethod()` and `parseProcessedDataMethod()` function was added.
* The `getHeightInformation()`function was added.

## Deprecated Classes and Functions
* The slot of the `RawData` class was renamed from `seaLevel` to `points`. It contains a data frame of distance, depth and resistivity values without topography information.
* The `XyzData` class was renamed to `ProcessedData`.
* The slot of the `ProcessedData` class was renamed from `seaLevel` to `points`. It contains a data frame of distance, depth and resistivity values without topography information.
* The slot of the `ProcessedData` class was renamed from `heightAdaption` to `pointsWithTopo`. It contains a data frame of distance, height and resistivity values with topography information.
* The method `plot3dXyz` was renamed to `plot3d`.
* The method `plotXyz` was renamed to `plotProcessedData`.
* The method `plotRaw` was renamed to `plotRawData`.
* The method `levelplotXyz` was renamed to `levelplotProcessedData`.
* The method `levelplotRaw` was renamed to `levelplotRawData`.
* The method `heightAdjustment` was renamed to `adjustHeight`.
* The method `plotXyzHeight` was renamed to `plotProcessedDataWithTopo`.
* The method `plotRawHeight` was renamed to `plotRawDataWithTopo`.
* The method `levelplotXyzHeight` was renamed to `levelplotProcessedDataWithTopo`.