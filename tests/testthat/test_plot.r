context('test_plot')

data(sinkhole)

test_that('Test Plotting Raw Data Without Topography', {
  plotRawData(sinkhole@profiles[[1]])
  plot(sinkhole@profiles[[1]],
       dataType = 'raw',
       withTopo = F)
})

test_that('Test Plotting Raw Data With Topography', {
  plotRawDataWithTopo(sinkhole@profiles[[1]])
  plot(sinkhole@profiles[[1]],
       dataType = 'raw',
       withTopo = T)
})

test_that('Test Plotting Processed Data Without Topography', {
  plotProcessedData(sinkhole@profiles[[1]])
  plot(sinkhole@profiles[[1]],
       dataType = 'processed',
       withTopo = F)
})

test_that('Test Plotting Processed Data With Topography', {
  plotProcessedDataWithTopo(sinkhole@profiles[[1]])
  plot(sinkhole@profiles[[1]],
       dataType = 'processed',
       withTopo = T)
})