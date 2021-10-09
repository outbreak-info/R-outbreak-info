#' @title Retrieve COVID-19 data for all countries
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all countries
#'
#' @return dataframe
#'
#' @examples
#' \dontrun{
#' getAdmn0()
#' }
#' @export

getAdmn0 <- function(...){
  data <- getEpiData(admin_level = 0, ...)
  return(data)
}
