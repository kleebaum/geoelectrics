# Parses a Raw Data File
parseRawDataFile <- function(address,
                             skip = 9) {
  con  <- file(address, open = 'r')
  
  numberOfRows <- 0
  
  for (i in 1:(1 + skip)) {
    oneLine <- readLines(con, n = 1)
  }
  
  while (grepl('.', oneLine, fixed = T)) {
    oneLine <- readLines(con, n = 1)
    numberOfRows <- numberOfRows + 1
  }
  
  close(con)
  
  rawData <- read.table(
    file = address,
    skip = skip,
    header = F,
    nrows = numberOfRows
  )
  
  result <- data.frame(rawData[1], rawData[2], rawData[4])
  colnames(result) <- c('dist', 'depth', 'val')
  result
}