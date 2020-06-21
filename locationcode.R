library(sf)
library(jsonlite)
library(ggplot2)

country_shp <- st_read(dsn="Downloads/ne_10m_admin_0_countries", "ne_10m_admin_0_countries")
state_shp <- st_read(dsn="Downloads/ne_10m_admin_1_states_provinces", "ne_10m_admin_1_states_provinces")
metro_shp <- st_read(dsn="Downloads/cb_2018_us_cbsa_500k", "cb_2018_us_cbsa_500k")
county_shp <- st_read(dsn="Downloads/tl_2019_us_county", "tl_2019_us_county")


county_shp$STATEFP=as.character(droplevels(county_shp$STATEFP))
stfipsnames <- read.csv("Downloads/stfipsnames.csv", colClasses="character")
county_shp=merge(county_shp, stfipsnames, all = TRUE)
county_shp$countystate_abrv <- paste(county_shp$NAMELSAD, county_shp$state_abrv, sep=", ")
#get county, st column

getExactNames <- function(loc_search){
  exact_names=c()
  loc_of_interest=c()
  for (i in loc_search){
    country_match = as.character(droplevels(subset(country_shp$ADMIN, grepl(paste(i, collapse= "|"), country_shp$ADMIN))))
    state_match = as.character(droplevels(subset(state_shp$name, grepl(paste(i, collapse= "|"), state_shp$name))))
    county_match = as.character(subset(county_shp$countystate_abrv, grepl(paste(i, collapse= "|"), county_shp$countystate_abrv)))
    metro_match = as.character(droplevels(subset(metro_shp$NAME, grepl(paste(i, collapse= "|"), metro_shp$NAME))))
    exact_names=c(exact_names, country_match, state_match, county_match, metro_match)
  }
  for (i in exact_names){
    print(i)
    loc_sel <- readline("Is this a location of interest? (Y/N): ")
    if ((loc_sel == "Y")|(loc_sel == "y")){
      loc_of_interest = c(loc_of_interest, i)
    }
    if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
      print("Expected input is Y or N")
      print(i)
      loc_sel <- readline("Is this a location of interest? (Y/N): ")
      if ((loc_sel == "Y")|(loc_sel == "y")){
        loc_of_interest = c(loc_of_interest, i)
      }
    }
  }
  return(loc_of_interest)
}
#search function

getLocationCode <- function(location_names){
  exact_loc = getExactNames(location_names)
  iso3codes=c()
  for (i in exact_loc){
    if (i %in% country_shp$ADMIN){
      countryiso3val=as.character(droplevels(country_shp$ADM0_A3[country_shp$ADMIN==i]))
      iso3codes=c(iso3codes, countryiso3val)
    }
    if (i %in% state_shp$name){
      iso3=levels(as.factor(as.character(state_shp$iso_3166_2[state_shp$name==i])))
      countryname=as.character(droplevels(state_shp$gu_a3[state_shp$iso_3166_2==iso3]))
      stateiso3val=paste0(countryname, "_", iso3)
      iso3codes=c(iso3codes, stateiso3val)
    }
    if (i %in% county_shp$countystate_abrv){
      sfips=county_shp$STATEFP[county_shp$countystate_abrv==i]
      cfips=as.character(droplevels(county_shp$COUNTYFP[county_shp$countystate_abrv==i]))
      fipsval=paste0(sfips,cfips)
      sfipsname=paste0("US", sfips)
      iso3=levels(as.factor(as.character(state_shp$iso_3166_2[state_shp$code_local==sfipsname])))
      countryname=as.character(droplevels(state_shp$gu_a3[state_shp$iso_3166_2==iso3]))
      countyiso3val=paste0(countryname, "_", iso3, "_", fipsval)
      iso3codes=c(iso3codes, countyiso3val)
    }
    if (i %in% metro_shp$NAME){
      mfips=as.character(droplevels(metro_shp$CBSAFP[metro_shp$NAME==i]))
      metroiso3val=paste0("METRO_", mfips)
      iso3codes=c(iso3codes, metroiso3val)
    }
  }
  return(iso3codes)
}

api.url <- "https://api.outbreak.info/v1/"

getLocationData <- function(loc_names){
  locations=getLocationCode(loc_names)
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

#bug when selecting two locations w/ similar names? ie San Diego metro & San Diego County

getAdmn2Data <- function(states_of_interest){
  locations=states_of_interest
  scroll.id <- NULL
  results <- list()
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=state_name:(",location.ids,")&fetch_all=true&sort=-date&admin_level=2")
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits);
}

plotDeaths <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, dead, color=location_id, group = location_id)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plotCases <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, confirmed, color=location_id, group = location_id)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}







