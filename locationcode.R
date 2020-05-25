library(rgdal)
library(tidyverse)
library(jsonlite)

country_shp <- readOGR(dsn="Downloads/ne_10m_admin_0_countries","ne_10m_admin_0_countries")
country_df=country_shp@data
county_shp <- readOGR(dsn="Downloads/tl_2019_us_county", "tl_2019_us_county")
county_df=county_shp@data
state_shp <- readOGR(dsn="Downloads/ne_10m_admin_1_states_provinces", "ne_10m_admin_1_states_provinces")
state_df=state_shp@data

getLocationCode <- function(locationnames){
  iso3codes=c()
  for (i in locationnames){
    if (i %in% country_df$ADMIN){
      countryiso3val=as.character(droplevels(country_df$ADM0_A3[country_df$ADMIN==i]))
      iso3codes=c(iso3codes,countryiso3val)
    }
    if (i %in% state_df$name){
      iso3=levels(as.factor(as.character(state_df$iso_3166_2[state_df$name==i])))
      countryname=as.character(droplevels(state_df$gu_a3[state_df$iso_3166_2==iso3]))
      stateiso3val=paste0(countryname, "_", iso3)
      iso3codes=c(iso3codes,stateiso3val)
    }
    if (i %in% county_df$NAMELSAD){
      sfips=as.character(droplevels(county_df$STATEFP[county_df$NAMELSAD==i]))
      cfips=as.character(droplevels(county_df$COUNTYFP[county_df$NAMELSAD==i]))
      fipsval=paste0(sfips,cfips)
      sfipsname=paste0("US", sfips)
      iso3=levels(as.factor(as.character(state_df$iso_3166_2[state_df$code_local==sfipsname])))
      countryname=as.character(droplevels(state_df$gu_a3[state_df$iso_3166_2==iso3]))
      countyiso3val=paste0(countryname, "_", iso3, "_", fipsval)
      iso3codes=c(iso3codes, countyiso3val)
    }
  }
  if (length(iso3codes)<length(locationnames)){
    print("One or more of the locations was not found")
  }
  if (length(iso3codes)>length(locationnames)){
    print("One or more of the locations is found more than once")
  }
return(iso3codes)
}
#must enter [county name] County

getLocationCode(c("India", "Virginia", "San Diego County"))
getLocationCode(c("Berkeley County", "Arizona"))
getLocationCode(c("Rusia", "Hawaii County"))
#testing

api.url <- "https://api.outbreak.info/v1/"

getLocationData <- function(location_names){
  locations=getLocationCode(location_names)
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
getLocationData(c("India", "Virginia", "San Diego County"))
