#' Parses a Processed Data File
#'
#' Parses .xyz files produced by the software Res2DInv.
#' Needs to be overwritten if another processed data format is used.
#'
#' @param address address of the raw data ascii file.
#' @param skip the number of lines of the data file to skip before beginning to read data.
#' @return list of two data frames:
#' The first data frame contains points without topography (distance, depth and resistivity values).
#' The second data frame contains points with topography (distance, height and resistivity values).
#' @export
#' @seealso \code{\link{ProcessedData-class}}, \code{\link{Profile-class}}
#' @examples
#' fileAddress <- system.file('extdata/processed/p1_DipolDipol_SW-NE.xyz',
#'                    package = 'geoelectrics')
#'
#' processedData = new('ProcessedData')
#' processedData@address = fileAddress
#' processedData@points <- parseProcessedDataFile(address = fileAddress)[[1]]
#' processedData@pointsWithTopo <- parseProcessedDataFile(address = fileAddress)[[2]]
parseProcessedDataFile <- function(address,
                                   skip = 0) {
  con  <- file(address, open = 'r')
  
  numberOfRows <- 0
  numberOfRows2 <- 0
  
  oneLine <- readLines(con, n = 1)
  while (grepl('/', oneLine)) {
    oneLine <- readLines(con, n = 1)
    skip <- skip + 1
  }
  
  while (!grepl('/', oneLine)) {
    oneLine <- readLines(con, n = 1)
    numberOfRows <- numberOfRows + 1
  }
  
  skipLines2 <- skip + numberOfRows
  
  while (grepl('/', oneLine)) {
    oneLine <- readLines(con, n = 1)
    skipLines2 <- skipLines2 + 1
  }
  
  while (!grepl('/', oneLine)) {
    oneLine <- readLines(con, n = 1)
    numberOfRows2 <- numberOfRows2 + 1
  }
  
  close(con)
  
  points <-
    read.table(
      file = address,
      skip = skip,
      header = F,
      nrows = numberOfRows
    )
  points <- data.frame(points[1], points[2], points[3])
  colnames(points) <- c('dist', 'depth', 'val')
  
  pointsWithTopo <- read.table(
    file = address,
    skip = skipLines2,
    header = F,
    nrows = numberOfRows2
  )
  pointsWithTopo <-
    data.frame(pointsWithTopo[1], pointsWithTopo[2], pointsWithTopo[3])
  colnames(pointsWithTopo) <- c('dist', 'height', 'val')
  
  list(points, pointsWithTopo)
}