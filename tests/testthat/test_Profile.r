context('test_Profile')

test_that('Test Profile Empty Constructor', {
  profile <- new('Profile')
  
  expect_identical(profile@number, 0)
  expect_equal(profile@title, '')
  expect_equal(profile@measurementType, '')
  expect_equal(profile@gpsCoordinates, new('GpsCoordinates'))
  expect_equal(profile@rawData, new('RawData'))
  expect_equal(profile@processedData, new('ProcessedData'))
})
