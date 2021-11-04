#' @title Retrieve COVID-19 data for counties in a state(s)
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all counties in one or more states/provinces.
#'
#' @param states vector or list of state name(s)
#'
#' @return dataframe
#'
#' @examples
#' getAdmn2ByState("California")
#'
#' @export

getAdmn2ByState <- function(states, ...){
  locations <- searchLocations(states, admin_level = 1)
  if (is.null(locations)){
    stop("No states selected")
  }
  data <- getEpiData(state_name = locations, admin_level = 2, ...)
  return(data)
}
