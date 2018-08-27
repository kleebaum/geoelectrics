context('test_ProfileSet')

data('sinkhole')

test_that('Test Profile Set Constructor No Profiles', {
  profileSet <- new('ProfileSet')
  
  expect_equal(profileSet@title, '')
  expect_equal(profileSet@profiles, list())
  expect_identical(profileSet@minData, 9999999)
  expect_identical(profileSet@maxData, 0)
  expect_identical(profileSet@minLat, 100000000000)
  expect_identical(profileSet@minLon, 100000000000)
})

test_that('Test Profile Set Constructor One Profile', {
  profileSet <-
    new('ProfileSet', list(sinkhole@profiles[[1]]), title = 'Sinkhole P1')
  
  expect_equal(profileSet@title, 'Sinkhole P1')
  expect_equal(profileSet@profiles,
               list(sinkhole@profiles[[1]]),
               check.attributes = F)
  expect_equal(profileSet@minData, 15.3, tolerance = 0.05)
  expect_equal(profileSet@maxData, 69075)
  expect_equal(profileSet@minLat, 5547507)
  expect_equal(profileSet@minLon, 653737)
})