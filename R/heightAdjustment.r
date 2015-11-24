#' Method to adjust height of a single profile
#' 
#' GPS measurements might differ otherwise.
#' 
#' @param Profile a single Profile
#' @param deltaMeter positive or negative value
#' @export
heightAdjustment <- function(Profile, deltaMeter) {
  Profile@xyzData@heightAdaption$depth <- 
    Profile@xyzData@heightAdaption$depth + deltaMeter
  return(Profile)
}