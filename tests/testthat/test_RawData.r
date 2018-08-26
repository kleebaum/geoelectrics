context("test_RawData")

test_that('Test RawData Constructor Missing Address', {
  expect_error(new('RawData')) # "argument \"address\" is missing, with no default"
})

test_that('Test RawData Constructor Empty Address', {
  expect_error(new('RawData', address = ''), "Raw data file cannot be found.")
})

test_that('Test RawData Constructor Wrong Address', {
  expect_error(new('RawData', address = 'abc')) # "cannot open the connection"
})

test_that('Test RawData Constructor Correct Address', {
  rawData = new('RawData', address = system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
                                                 package = 'geoelectrics'))
  expect_s4_class(rawData, "RawData")
  expect_equal(rawData, initialize(rawData, address = system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
                                                                  package = 'geoelectrics')))
})