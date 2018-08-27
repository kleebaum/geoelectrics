#' Parses a Raw Data File
#'
#' Parses a geoelectrics raw data file created by the GeoTest software by Dr. Rauen.
#' Needs to be overwritten if another raw data format is used.
#'
#' @param address address of the raw data ascii file.
#' @param skip the number of lines of the data file to skip before beginning to read data.
#' @return data frame containing distance, depth and resistivity values
#' @export
#' @seealso \code{\link{RawData-class}}, \code{\link{Profile-class}}
#' @examples
#' fileAddress <- system.file('extdata/raw/p1_DipolDipol_SW-NE.dat',
#'                    package = 'geoelectrics')
#'                    
#' rawData = new('RawData')
#' rawData@address = fileAddress
#' rawData@points <- parseRawDataFile(address = fileAddress)
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