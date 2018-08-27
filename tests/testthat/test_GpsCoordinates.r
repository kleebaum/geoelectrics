context('test_GpsCoordinates')

testFileAddress <- system.file('extdata/gps/p1.txt',
                               package = 'geoelectrics')

test_that('Test GpsCoordinates Constructor Missing Address', {
  # 'argument \'address\' is missing, with no default'
  expect_that(gpsCoordinates <- new('GpsCoordinates'),
              prints_text('Created an empty GPS coordinates object.'))
  
  expect_equal(gpsCoordinates@exact, data.frame(lat = double(),
                                                lon = double()))
})

test_that('Test GpsCoordinates Constructor Empty Address', {
  expect_error(
    new('GpsCoordinates', address = ''),
    'GPS coordinates file address is given but file cannot be found.'
  )
})

test_that('Test GpsCoordinates Constructor Wrong Address', {
  # 'cannot open the connection'
  expect_error(new('GpsCoordinates', address = 'abc'))
})

test_that('Test GpsCoordinates Constructor Correct Address', {
  gpsCoordinates = new('GpsCoordinates', address = testFileAddress)
  expect_s4_class(gpsCoordinates, 'GpsCoordinates')
  expect_equal(gpsCoordinates,
               initialize(gpsCoordinates, address = testFileAddress))
  lm <- lm(gpsCoordinates@exact$lat ~ gpsCoordinates@exact$lon)
  expect_equal(lm$fitted.values, gpsCoordinates@lm$fitted.values)
})

test_that('Test GpsCoordinates From Gauss Krueger', {
  sampleCoordinates <- data.frame(lat = 5, lon = 2)
  gpsCoordinates = new('GpsCoordinates')
  gpsCoordinates@exact <- sampleCoordinates
  expect_equal(calcRelativeCoords(gpsCoordinates, 5, 2), data.frame(lat = 0, lon = 0))
})