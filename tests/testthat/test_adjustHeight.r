context("test_adjustHeight")

test_that("Test adding no height", {
  data(sinkhole)
  expect_equal(sinkhole@profiles[[1]], adjustHeight(sinkhole@profiles[[1]], 0))
})

test_that("Test adding height works", {
  data(sinkhole)
  expect_s4_class(adjustHeight(sinkhole@profiles[[1]], 10), "Profile")
  p_new <- adjustHeight(sinkhole@profiles[[1]], 10)
  expect_identical(p_new@processedData@pointsWithTopo$height, sinkhole@profiles[[1]]@processedData@pointsWithTopo$height + 10)
})