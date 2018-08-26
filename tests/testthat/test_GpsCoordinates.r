test_that('Test GpsCoordinates Constructor Missing Address', {
  expect_error(new('GpsCoordinates'), "argument \"address\" is missing, with no default")
})

test_that('Test GpsCoordinates Constructor Empty Address', {
  expect_error(new('GpsCoordinates', address = ''), "GPS coordinates file address is empty.")
})

test_that('Test GpsCoordinates Constructor Wrong Address', {
  expect_error(new('GpsCoordinates', address = 'abc'), "cannot open the connection")
})

# test_that('Test GpsCoordinates Constructor Correct Address', {
#  gpsCoordinates = new('GpsCoordinates', address = system.file('extdata/gps/p1.txt',
#                                                                package='geoelectrics'))
#  expect_s4_class(gpsCoordinates, "GpsCoordinates")
# })