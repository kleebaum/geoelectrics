context('test_plot3d')

data(sinkhole)

test_that('Test Plotting 3D Single Profile', {
  plot3dXyz(sinkhole@profiles[[1]])
})
