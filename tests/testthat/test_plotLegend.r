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

test_that('Test Plotting Legend of Single Profile With Custom Lab Breaks', {
  plot.new()
  plotLegend(
    sinkhole@profiles[[3]],
    horizontal = F,
    lab.breaks = c(10, 100, 500, 5000, 15000)
  )
})