context('test_plotLegend')

data(sinkhole)

test_that('Test Plotting Legend of Single Profile', {
  plot.new()
  plotLegend(sinkhole@profiles[[3]], horizontal = F)
})

test_that('Test Plotting Legend of Profile Set', {
  plot.new()
  plotLegend(sinkhole)
})