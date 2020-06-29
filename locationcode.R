library(jsonlite)
library(ggplot2)
library(readr)

api.url <- "https://api.outbreak.info/v1/"

getExactLocations <- function(locations_to_search){
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
    dataurl <- paste0(api.url, "query?q=(",location.ids,")%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  df=(hits)
  df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
  df
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
  return(locs_of_interest)
}

getLocationData <- function(loc_names){
  locations=getExactLocations(loc_names)
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


la_df=getAdmn2Data("Louisiana")
countypop=as.data.frame(read_csv("Downloads/co-est2019-alldata.csv"))
countypop=subset(countypop, select=c("CTYNAME", "STNAME", "POPESTIMATE2019"))
la_df=merge(x=la_df, y=countypop, by.x=c("name", "state_name"), by.y=c("CTYNAME", "STNAME"))


