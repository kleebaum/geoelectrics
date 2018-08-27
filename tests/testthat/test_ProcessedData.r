context('test_ProcessedData')

testFileAddress <- system.file('extdata/processed/p1_DipolDipol_SW-NE.xyz',
                               package = 'geoelectrics')

test_that('Test ProcessedData Constructor Missing Address', {
  # 'argument \'address\' is missing, with no default'
  expect_that(new('ProcessedData'), prints_text('Created an empty processed data object.')) 
})

test_that('Test ProcessedData Constructor Empty Address', {
  expect_error(new('ProcessedData', address = ''), 'Processed data file address is given but file cannot be found.')
})

test_that('Test ProcessedData Constructor Wrong Address', {
  # 'cannot open the connection'
  expect_error(new('ProcessedData', address = 'abc')) 
})

test_that('Test ProcessedData Constructor Correct Address', {
  processedData = new('ProcessedData', address = testFileAddress)
  expect_s4_class(processedData, 'ProcessedData')
  expect_equal(processedData, initialize(processedData, address = testFileAddress))
})

test_that('Test Parse Processed Data File Correct Address', {
  processedData = new('ProcessedData')
  processedData@address = testFileAddress
  processedData@points <- parseProcessedDataFile(address = testFileAddress)[[1]]
  processedData@pointsWithTopo <- parseProcessedDataFile(address = testFileAddress)[[2]]
  processedData@minData <- min(processedData@pointsWithTopo[3])
  processedData@maxData <- max(processedData@pointsWithTopo[3])
  processedData@height <- getHeightInformation(processedData)
  
  expect_equal(processedData, new('ProcessedData', address = testFileAddress))
})
