library(jsonlite)
library(ggplot2)

api.url <- "https://api.outbreak.info/v1/"

getISO3 <- function(locations_to_search){
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
  t <- try(rbind_pages(results), silent=T)
  if("try-error" %in% class(t)){
    error=T
  }else{
    hits <- rbind_pages(results)
    df=(hits)
    locs_of_interest=df$location_id
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(locs_of_interest)
  }
  if (length(locs_of_interest)!=length(locations_to_search)||error==T){
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
      dataurl <- gsub(" ", "+", dataurl)
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

searchLocations <- function(locations_to_search, admin_level){
  locs_of_interest=c()
  scroll.id <- NULL
  location.ids <- paste0("(name:%22", paste(locations_to_search, collapse="%22%20OR%20name:%22"), "%22)")
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
  t <- try(rbind_pages(results), silent=T)
  if("try-error" %in% class(t)){
    error=T
  }else{
    hits <- rbind_pages(results)
    df=(hits)
    locs_of_interest=df$location_id
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(df$name)
  }
  if ((length(locs_of_interest)!=length(locations_to_search))|error==T){
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
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
    }
    hits <- rbind_pages(results)
    df=hits
    df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
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

getLocationCodes <- function(location_names, admin_level){
  locations <- searchLocations(location_names, admin_level)
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

getLocationData <- function(location_names){
  locations <- getISO3(location_names)
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

getAdmn1ByCountry <- function(countries){
  locations <- searchLocations(countries, admin_level=0)
  scroll.id <- NULL
  results <- list()
  location.ids <- paste0("%22", paste(locations, collapse="%22%20OR%20%22"), "%22")
  success <- NULL
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=country_name:(",location.ids,")&fetch_all=true&sort=-date&admin_level=1")
    dataurl <- gsub(" ", "+", dataurl)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits)
}

getMetroByCountry <- function(){
  scroll.id <- NULL
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- "https://api.outbreak.info/covid19/query?q=country_name:%22United%20States%20of%20America%22%20AND%20admin_level:1.5&fetch_all=true"
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    resp <- fromJSON(dataurl, flatten=TRUE)
    scroll.id <- resp$'_scroll_id'
    results[[length(results) + 1]] <- resp$hits
    success <- resp$success
  }
  hits <- rbind_pages(results)
  return(hits)
}

plotCovid <- function(locations, key){
  df <- getLocationData(locations)
  df$date=as.Date(df$date, "%Y-%m-%d")
  p=ggplot(df, aes(date, get(key), color=name, group = name)) + geom_line() + scale_x_date(date_breaks = "1 week") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y=key)
}


