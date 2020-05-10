library(rgdal)

admn0_shp <- readOGR(dsn = "Downloads/ne_10m_admin_0_countries","ne_10m_admin_0_countries")

admn0_df=admn0_shp@data

getCountryCode <- function(countrynames){
  iso3=c()
  for (i in countrynames){
    iso3val=as.character(droplevels(admn0_df$ADM0_A3[admn0_df$ADMIN==i]))
    iso3=c(iso3,iso3val)
  }
  return(iso3)
}

#test
print(getCountryCode(list('India', 'Argentina')))

library(tidyverse)
library(jsonlite)

api.url <- "https://api.outbreak.info/v1/"

getCountryData <- function(location_names){
  locations=getCountryCode(location_names)
  scroll.id <- NULL
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=location_id:(",location.ids,")&sort=date&size=1000&fetch_all=true")
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits);
}

#test
df <- getCountryData(c('India','Argentina','Iraq'))
df


