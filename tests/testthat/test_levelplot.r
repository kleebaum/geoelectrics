context('test_levelplot')

data(sinkhole)

test_that('Test Plotting Raw Data Without Topography', {
  levelplotRawData(sinkhole@profiles[[1]])
  levelplot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = FALSE)
  expect_error(levelplotLegendLabel())
})

test_that('Test Plotting Raw Data With Topography', {
  levelplot(sinkhole@profiles[[1]], dataType = 'raw', withTopo = TRUE)
})

test_that('Test Plotting Processed Data Without Topography', {
  levelplotProcessedData(sinkhole@profiles[[1]])
  levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = FALSE)
  expect_error(levelplotLegendLabel())
})

test_that('Test Plotting Processed Data With Topography', {
  levelplotProcessedDataWithTopo(sinkhole@profiles[[1]])
  levelplot(sinkhole@profiles[[1]], dataType = 'processed', withTopo = TRUE)
  expect_error(levelplotLegendLabel())
})

test_that('Test Plotting Profile Set With Topography', {
  levelplot(sinkhole,
       dataType = 'processed',
       withTopo = T)
})