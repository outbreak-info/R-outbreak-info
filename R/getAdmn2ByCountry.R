#' @title Retrieve COVID-19 data for all counties in the US.
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all counties in the United States of America.
#'
#' @return dataframe
#'
#' @examples
#' # Takes a long time to run!
#' \dontrun{
#' getAdmn2ByCountry()
#' }
#'

getAdmn2ByCountry <- function(...){
  data <- getEpiData(country_name = "United States of America", admin_level = 2, ...)
  return(data)
}
