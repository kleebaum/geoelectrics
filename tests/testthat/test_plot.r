context('test_plot')

data(sinkhole)

test_that('Test Plotting Raw Data Without Topography', {
  plotRaw(sinkhole@profiles[[1]])
})

test_that('Test Plotting Raw Data With Topography', {
  plotRawHeight(sinkhole@profiles[[1]])
})

test_that('Test Plotting Processed Data Without Topography', {
  plotXyz(sinkhole@profiles[[1]])
})

test_that('Test Plotting Processed Data With Topography', {
  plotXyzHeight(sinkhole@profiles[[1]])
})