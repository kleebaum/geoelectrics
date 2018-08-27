context('test_levelplot')

data(sinkhole)

test_that('Test Plotting Raw Data Without Topography', {
  levelplotRaw(sinkhole@profiles[[1]])
  expect_error(levelplotLegendLabel())
})

# test_that('Test Plotting Raw Data With Topography', {
#   levelplotRawHeight(sinkhole@profiles[[1]])
# })

test_that('Test Plotting Processed Data Without Topography', {
  levelplotXyz(sinkhole@profiles[[1]])
  expect_error(levelplotLegendLabel())
})

test_that('Test Plotting Processed Data With Topography', {
  levelplotXyzHeight(sinkhole@profiles[[1]])
  expect_error(levelplotLegendLabel())
})
