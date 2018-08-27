context('test_RawData')

testFileAddress <- system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
                               package = 'geoelectrics')

test_that('Test RawData Constructor Missing Address', {
  # 'argument \'address\' is missing, with no default'
  expect_that(new('RawData'), prints_text('Created an empty raw data object.')) 
})

test_that('Test RawData Constructor Empty Address', {
  expect_error(new('RawData', address = ''), 'Raw data file address is given but file cannot be found.')
})

test_that('Test RawData Constructor Wrong Address', {
  # 'cannot open the connection'
  expect_error(new('RawData', address = 'abc')) 
})

test_that('Test Parse Raw Data File Correct Address', {
  rawData = new('RawData')
  rawData@address = testFileAddress
  rawData@points <- parseRawDataFile(address = testFileAddress)
  expect_equal(rawData, new('RawData', address = testFileAddress))
})

test_that('Test RawData Constructor Correct Address', {
  rawData = new('RawData', address = testFileAddress)
  expect_s4_class(rawData, 'RawData')
  expect_equal(rawData, initialize(rawData, address = testFileAddress))
})