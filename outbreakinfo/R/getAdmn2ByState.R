#' @title Retrieve COVID-19 data for counties in a state(s)
#'
#' @description Retrieve up-to-date COVID-19 data from outbreak.info for all counties in one or more states/provinces.
#'
#' @param states: vector or list of state name(s)
#'
#' @return dataframe
#'
#' @examples
#' getAdmn2ByState("California")
#'
#' @export
#' @import jsonlite

getAdmn2ByState <- function(states){
  locations <- searchLocations(states, admin_level=1)
  scroll.id <- NULL
  results <- list()
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=state_name:(",location.ids,")&fetch_all=true&sort=-date&admin_level=2")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits);
}
