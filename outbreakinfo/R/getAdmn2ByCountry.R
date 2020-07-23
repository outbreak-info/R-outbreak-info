#' @title Retrieve COVID-19 data for all counties in the US.
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all counties in the United States of America.
#'
#' @return dataframe
#'
#' @examples
#' getAdmn2ByCountry()
#'
#' @export
#' @import jsonlite

getAdmn2ByCountry <- function(){
  scroll.id <- NULL
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- "https://api.outbreak.info/covid19/query?q=country_name:%22United%20States%20of%20America%22%20AND%20admin_level:2&fetch_all=true"
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits)
}
