context('test_plotIntersect')

data(sinkhole)

test_that('Test Plotting Intersection of Two Profiles', {
  plotIntersect(sinkhole@profiles[[1]], sinkhole@profiles[[2]])
})

test_that('Test Plotting Intersection of All Profiles in a Profile Set', {
  plotIntersect(sinkhole)
})