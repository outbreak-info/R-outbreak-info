library(jsonlite)
library(ggplot2)
library(readr)
library(sf)

api.url <- "https://api.outbreak.info/v1/"

country_shp <- st_read(dsn="Downloads/ne_10m_admin_0_countries", "ne_10m_admin_0_countries")
state_shp <- st_read(dsn="Downloads/ne_10m_admin_1_states_provinces", "ne_10m_admin_1_states_provinces")
metro_shp <- st_read(dsn="Downloads/cb_2018_us_cbsa_500k", "cb_2018_us_cbsa_500k")
county_shp <- st_read(dsn="Downloads/tl_2019_us_county", "tl_2019_us_county")

stfipsnames <- read.csv("Downloads/stfipsnames.csv", colClasses="character")
county_shp$STATEFP=as.character(droplevels(county_shp$STATEFP))
county_shp=merge(county_shp, stfipsnames, all = TRUE)

countypop=as.data.frame(read_csv("Downloads/co-est2019-alldata.csv"))
statepop=subset(countypop, countypop$SUMLEV=="040")
statepop=subset(statepop, select=c("STNAME", "POPESTIMATE2019"))

countypop=subset(countypop, countypop$SUMLEV=="050")
countypop=subset(countypop, select=c("CTYNAME", "STNAME", "POPESTIMATE2019"))
row.names(countypop) <- NULL
countypop[grep("Ana County", countypop$CTYNAME), ]
countypop$CTYNAME[1803]="Doña Ana County"

metropop=as.data.frame(read_csv("Downloads/cbsa-est2019-alldata.csv"))
metropop=subset(metropop, select=c("NAME", "POPESTIMATE2019"))

state_shp$name=as.character(droplevels(state_shp$name))
statepop_shp=merge(x=state_shp, y=statepop, by.x=c("name"), by.y=c("STNAME"), all.x = TRUE)

subset(statepop_shp, (!is.na(statepop_shp$POPESTIMATE2019)))$name

which(state_shp$name=="Maryland")

countypop_shp=merge(x=county_shp, y=countypop, by.x=c("NAMELSAD", "state_name"), by.y=c("CTYNAME", "STNAME"), all.x = TRUE)

metropop_shp=merge(x=metro_shp, y=metropop, by="NAME", all.x = TRUE)

countypop_shp = countypop_shp[!st_is_empty(countypop_shp),,drop=FALSE]

#st_write(countypop_shp, "Downloads/countypop/countypop.shp")
#st_write(statepop_shp, "Downloads/statepop/statepop.shp")
#st_write(metropop_shp, "Downloads/metropop/metropop.shp")

getExactLocations <- function(locations_to_search){
  locs_of_interest=c()
  scroll.id <- NULL
  location.ids <- paste0("(name:%22", paste(locations_to_search, collapse="%22%20OR%20name:%22"), "%22)")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  df=(hits)
  locs_of_interest=df$location_id
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    locs_of_interest=c()
    for (i in locations_to_search){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    scroll.id <- NULL
    location.ids <- paste0("(name:", paste(locations, collapse="%20OR%20name:"), ")")
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
    }
    hits <- rbind_pages(results)
    df=(hits)
    df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
    for (i in df$name){
      print(i)
      loc_sel <- readline("Is this a location of interest? (Y/N): ")
      if ((loc_sel == "Y")|(loc_sel == "y")){
        locs_of_interest = c(locs_of_interest, df$location_id[df$name==i])
      }
      if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
        print("Expected input is Y or N")
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$location_id[df$name==i])
        }
      }
    }
  }
  return(locs_of_interest)
}

getExactNames <- function(locations_to_search, admin_level){
  locs_of_interest=c()
  scroll.id <- NULL
  location.ids <- paste0("(name:", paste(locations_to_search, collapse="%20OR%20name:"), ")")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=admin_level:", admin_level, "%20AND%20(",location.ids,")%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  df=hits
  locs_of_interest=df$name
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    locs_of_interest=c()
    for (i in locations_to_search){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    scroll.id <- NULL
    location.ids <- paste0("(name:", paste(locations, collapse="%20OR%20name:"), ")")
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=admin_level:", admin_level, "%20AND%20(",location.ids,")%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
    }
    hits <- rbind_pages(results)
    df=hits
    for (i in df$name){
      print(i)
      loc_sel <- readline("Is this a location of interest? (Y/N): ")
      if ((loc_sel == "Y")|(loc_sel == "y")){
        locs_of_interest = c(locs_of_interest, i)
      }
      if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
        print("Expected input is Y or N")
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, i)
        }
      }
    }
  }
  return(locs_of_interest)
}

getLocationCodes <- function(loc_names, admin_level){
  locations=getExactNames(loc_names, admin_level)
  scroll.id <- NULL
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=name:(",location.ids,")&fetch_all=true&sort=-date&fields=location_id")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  df=hits
  iso3=unique(df$location_id)
  return(iso3)
}

getLocationData <- function(loc_names){
  locations=getExactLocations(loc_names)
  scroll.id <- NULL
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=location_id:(",location.ids,")&sort=date&size=1000&fetch_all=true")
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


getAdmn2Data <- function(states_of_interest){
  locations=getExactNames(states_of_interest, admin_level=1)
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


plotDeaths <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, dead, color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plotCases <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, confirmed, color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plotCasesPer100k <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, confirmed_per_100k, color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

plotDeathsPer100k <- function(locs){
  df <- getLocationData(locs)
  df$date=as.Date(df$date, "%Y-%m-%d")
  ggplot(df, aes(date, dead_per_100k, color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
}


countypop=as.data.frame(read_csv("Downloads/co-est2019-alldata.csv"))
countypop=subset(countypop, countypop$SUMLEV=="050")
countypop=subset(countypop, select=c("CTYNAME", "STNAME", "POPESTIMATE2019"))
row.names(countypop) <- NULL
countypop[grep("Ana County", countypop$CTYNAME), ]
countypop$CTYNAME[1803]="Doña Ana County"


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
usa_df = hits

usa_df=merge(x=usa_df, y=countypop, by.x=c("name", "state_name"), by.y=c("CTYNAME", "STNAME"), all.x = TRUE)
usa_df=transform(usa_df, incidencePer1k = (confirmed/POPESTIMATE2019)*1000)
#write.csv(usa_df, "Downloads/usa_df.csv", row.names=F)

all_la_df=getAdmn2Data("Louisiana")
all_la_df=merge(x=la_df, y=countypop, by.x=c("name", "state_name"), by.y=c("CTYNAME", "STNAME"),  all.x = TRUE)
#write.csv(all_la_df, "Downloads/all_la_df.csv", row.names=F)

la_df=getLocationData(c("New Orleans", "Shreveport-Bossier", "Caddo Parish", "East Baton Rouge"))
la_df=merge(x=la_df, y=countypop, by.x=c("name", "state_name"), by.y=c("CTYNAME", "STNAME"), all.x = TRUE)
metropop=as.data.frame(read_csv("Downloads/cbsa-est2019-alldata.csv"))
metropop=subset(metropop, select=c("NAME", "POPESTIMATE2019"))
la_df=merge(x=la_df, y=metropop, by.x=c("name"), by.y=c("NAME"), all.x = TRUE)

la_df$POPESTIMATE2019=pmin(la_df$POPESTIMATE2019.x, la_df$POPESTIMATE2019.y, na.rm = T)
la_df=within(la_df, rm(POPESTIMATE2019.x, POPESTIMATE2019.y))

la_df=transform(la_df, incidencePer1k = (confirmed/POPESTIMATE2019)*1000)

colnames(la_df)
la_df$date=as.Date(la_df$date, "%Y-%m-%d")
ggplot(la_df, aes(date, incidencePer1k, color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(la_df, aes(date, confirmed, color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(la_df, aes(date, confirmed_numIncrease, color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(la_df, aes(date, confirmed_rolling, color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

globalpop=as.data.frame(read_csv("Downloads/gpw-v4-admin-unit-center-points-population-estimates-rev11_global_csv/gpw_v4_admin_unit_center_points_population_estimates_rev11_global.csv"))

