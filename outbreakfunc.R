library(jsonlite)
library(ggplot2)
library(readr)
library(progress)
library(plyr)
library(stringr)

api.url <- "https://api.outbreak.info/covid19/"

getISO3 <- function(locations_to_search){
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("(name:%22", paste(i, collapse="%22%20OR%20name:%22"), "%22)")
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
      if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
        stop("Could not connect to API. Check internet connection and try again.")
      }else{
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
      }
    }
    t <- try(rbind_pages(results), silent=T)
    if("try-error" %in% class(t)){
      error=T
      locs_not_found = c(locs_not_found, i)
    }else{
      hits <- rbind_pages(results)
      df=(hits)
      if (nrow(df)==1){
        locs_of_interest=c(locs_of_interest, df$location_id)
      }else{
        locs_not_found=c(locs_not_found, i)
      }
    }
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(locs_of_interest)
  }
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    for (i in locs_not_found){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    for (i in 1:length(locations)){
      scroll.id <- NULL
      location.ids <- paste0("(name:", paste(locations[i], collapse="%20OR%20name:"), ")")
      location.ids <- gsub("&", "%26", location.ids)
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(api.url, "query?q=",location.ids,"%20AND%20mostRecent:true&fields=name,location_id,state_name,admin_level&fetch_all=true")
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
        if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
          stop("Could not connect to API. Check internet connection and try again.")
        }else{
        resp <- fromJSON(dataurl, flatten=TRUE)
        scroll.id <- resp$'_scroll_id'
        results[[length(results) + 1]] <- resp$hits
        success <- resp$success
        }
      }
      t2 <- try(rbind_pages(results), silent=T)
      if("try-error" %in% class(t2)){
        print(paste(locs_not_found[i], "not found. Please check spelling."))
        next
      }else{
        hits <- rbind_pages(results)
        df=(hits)
        df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
        df$admin_level[df$admin_level == "-1"] <- "World Bank Region"
        df$admin_level[df$admin_level == "0"] <- "country"
        df$admin_level[df$admin_level == "1"] <- "state/province"
        df$admin_level[df$admin_level == "1.5"] <- "metropolitan area"
        df$admin_level[df$admin_level == "2"] <- "county"
        df$fullname <- paste0(df$name, " (", df$admin_level, ")")
      }
      for (i in df$fullname){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$location_id[df$fullname==i])
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, df$location_id[df$fullname==i])
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}

searchLocations <- function(locations_to_search, admin_level){
  if (missing(admin_level)){
    stop("Administrative level not specified")
  }
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("(name:%22", paste(i, collapse="%22%20OR%20name:%22"), "%22)")
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(api.url, "query?q=", location.ids, "%20AND%20", "admin_level:%22", admin_level, "%22", "%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
      if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
        stop("Could not connect to API. Check internet connection and try again.")
      }else{
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
      }
    }
    t <- try(rbind_pages(results), silent=T)
    if("try-error" %in% class(t)){
      error=T
      locs_not_found = c(locs_not_found, i)
    }else{
      hits <- rbind_pages(results)
      df=(hits)
      if (nrow(df)==1){
        locs_of_interest=c(locs_of_interest, i)
      }else{
        locs_not_found=c(locs_not_found, i)
      }
    }
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(locs_of_interest)
  }
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    for (i in locs_not_found){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    for (i in 1:length(locations)){
      scroll.id <- NULL
      location.ids <- paste0("(name:", paste(locations[i], collapse="%20OR%20name:"), ")")
      location.ids <- gsub("&", "%26", location.ids)
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(api.url, "query?q=", location.ids, "%20AND%20", "admin_level:%22", admin_level, "%22", "%20AND%20mostRecent:true&fields=name,location_id,state_name&fetch_all=true")
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
        if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
          stop("Could not connect to API. Check internet connection and try again.")
        }else{
        resp <- fromJSON(dataurl, flatten=TRUE)
        scroll.id <- resp$'_scroll_id'
        results[[length(results) + 1]] <- resp$hits
        success <- resp$success
        }
      }
      t2 <- try(rbind_pages(results), silent=T)
      if("try-error" %in% class(t2)){
        print(paste(locs_not_found[i], "not found. Please check spelling."))
        next
      }else{
        hits <- rbind_pages(results)
        df=(hits)
        df$name=apply(cbind(df$name, df$state_name), 1, function(x) paste(x[!is.na(x)], collapse = ", "))
      }
      for (i in df$name){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, i)
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, i)
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}

getEpiData <- function(name=NULL, location_id=NULL, wb_region=NULL, country_name=NULL, state_name=NULL, admin_level=NULL, date=NULL, mostRecent=NULL, fields=NULL, sort=NULL, size=1000){
  q <- c()
  if(!is.null(name)){
    q <- c(q, paste0("(name:%22", paste(name, collapse="%22%20OR%20name:%22"), "%22)%20AND%20"))
  }
  if(!is.null(location_id)){
    q <- c(q, paste0("(location_id:%22", paste(location_id, collapse="%22%20OR%20location_id:%22"), "%22)%20AND%20"))
  }
  if(!is.null(wb_region)){
    q <- c(q, paste0("(wb_region:%22", paste(wb_region, collapse="%22%20OR%20wb_region:%22"), "%22)%20AND%20"))
  }
  if(!is.null(country_name)){
    q <- c(q, paste0("(country_name:%22", paste(country_name, collapse="%22%20OR%20country_name:%22"), "%22)%20AND%20"))
  }
  if(!is.null(state_name)){
    q <- c(q, paste0("(state_name:%22", paste(state_name, collapse="%22%20OR%20state_name:%22"), "%22)%20AND%20"))
  }
  if(!is.null(admin_level)){
    q <- c(q, paste0("(admin_level:%22", paste(admin_level, collapse="%22%20OR%20admin_level:%22"), "%22)%20AND%20"))
  }
  if(!is.null(date)){
    if (!is.character(date)){
      stop("Date must be in string format")
    }else{
      q <- c(q, paste0("(date:%22", paste(date, collapse="%22%20OR%20date:%22"), "%22)%20AND%20"))
    }
  }
  q <- paste(q, sep="", collapse = "")
  q <- substr(q, 1, nchar(q)-9)
  if(!is.null(mostRecent)){
    if (!is.logical(mostRecent)){
      stop("mostRecent must be in Boolean format")
    }else{
      q <- c(q, paste0("%20AND%20", "mostRecent:", tolower(mostRecent)))
    }
  }
  q <- paste(q, sep="", collapse = "")
  q <- gsub("&", "%26", q)
  if(!is.null(fields)){
    q <- c(q, paste0("&fields=", paste(fields, collapse=",")))
  }
  if(!is.null(sort)){
    q <- c(q, paste0("&sort=", paste(sort)))
  }
  
  q <- c(q, paste0("&size=", paste(size)))
  q <- paste(q, sep="", collapse = "")
  q <- paste0(q, "&fetch_all=true")
  q <- gsub(" ", "+", q)
  
  dataurl <- paste0(api.url, "query?q=", q)
  t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
  if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
    stop("Could not connect to API. Check internet connection and try again.")
  }else{
    resp <- fromJSON(dataurl, flatten=TRUE)
  }
  max <- resp$total
  
  scroll.id <- NULL
  results <- list()
  success <- NULL
  pb <- progress_bar$new(
    format = "  downloading [:bar] :percent eta: :eta",
    total = max, clear = FALSE, width= 60)
  pb$tick(0)
  while(is.null(success)){
    dataurl <- paste0(api.url, "query?q=", q)
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
    if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
      stop("Could not connect to API. Check internet connection and try again.")
    }else{
      resp <- fromJSON(dataurl, flatten=TRUE)
      scroll.id <- resp$'_scroll_id'
      results[[length(results) + 1]] <- resp$hits
      success <- resp$success
      if (is.null(success)){
        pb$tick(size)
      }else{
        pb$finished <- T
      }
    }
  }
  pb$terminate()
  if(length(results) > 1){
    hits <- rbind_pages(results)
  }else{
    hits <- data.frame(results)
  }
  if ("date" %in% colnames(hits)){
    hits$date=as.Date(hits$date, "%Y-%m-%d")
    hits <- hits[order(as.Date(hits$date, format = "%Y-%m-%d")),]
  }
  return(hits)
}
#update getEpiData in package --> included sorting for dates

getLocationData <- function(location_names, ...){
  location_codes <- getISO3(location_names)
  data <- getEpiData(location_id=location_codes, ...)
  return(data)
}

getAdmn2ByState <- function(states, ...){
  locations <- searchLocations(states, admin_level = 1)
  if (is.null(locations)){
    stop("No states selected")
  }
  data <- getEpiData(state_name = locations, admin_level = 2, ...)
  return(data)
}

getAdmn2ByCountry <- function(...){
  data <- getEpiData(country_name = "United States of America", admin_level = 2, ...)
  return(data)
}

getAdmn1ByCountry <- function(countries, ...){
  locations <- searchLocations(countries, admin_level = 0)
  if (is.null(locations)){
    stop("No countries selected")
  }
  data <- getEpiData(country_name = locations, admin_level = 1, ...)
  return(data)
}

getAdmn0 <- function(...){
  data <- getEpiData(admin_level = 0, ...)
  return(data)
}

getMetroByCountry <- function(...){
  data <- getEpiData(country_name = "United States of America", admin_level = 1.5, ...)
  return(data)
}

getCountryByRegion <- function(wb_regions, ...){
  locations <- searchLocations(wb_regions, admin_level = -1)
  if (is.null(locations)){
    stop("No regions selected")
  }
  data <- getEpiData(wb_region = locations, admin_level = 0, ...)
  return(data)
}

getByAdmnLevel <- function(admin_level, ...){
  data <- getEpiData(admin_level = admin_level, ...)
  return(data)
}

plotCovid <- function(locations, variable){
  if (missing(variable)){
    stop("Variable to plot not specified")
  }
  location_codes <- getISO3(locations)
  df <- getEpiData(location_id=location_codes)
  if (!(variable %in% colnames(df))){
    print(paste(variable, "is not a valid API field"))
    return(NULL)
  }
  p=ggplot(df, aes(date, get(variable), color=name, group=name)) + geom_line() + scale_x_date(date_breaks = "1 month") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + labs(y=variable)
  return(p)
}
#update error message using stop()

printAPIFields <- function(){
  df=data.frame("API Field"=c("admin_level", "cbsa", "confirmed", "confirmed_doublingRate", "confirmed_firstDate", "confirmed_newToday", "confirmed_numIncrease", "confirmed_pctIncrease", "confirmed_per_100k", "confirmed_rolling", "confirmed_rolling_14days_ago", "confirmed_rolling_14days_ago_diff", "confirmed_rolling_per_100k", "country_gdp_per_capita", "country_iso3", "country_name", "country_population", "date", "daysSince100Cases", "daysSince10Deaths", "daysSince50Deaths", "dead", "dead_doublingRate", "dead_firstDate", "dead_newToday", "dead_numIncrease", "dead_pctIncrease", "dead_per_100k", "dead_rolling", "dead_rolling_14days_ago", "dead_rolling_14days_ago_diff", "dead_rolling_per_100k", "first_dead-first_confirmed", "gdp_last_updated", "gdp_per_capita", "iso3", "lat", "location_id", "long", "mostRecent", "name", "num_subnational", "population", "recovered", "recovered_doublingRate", "recovered_firstDate", "recovered_newToday", "recovered_numIncrease", "recovered_pctIncrease", "recovered_per_100k", "recovered_rolling", "recovered_rolling_14days_ago", "recovered_rolling_14days_ago_diff", "recovered_rolling_per_100k", "state_iso3", "state_name", "sub_parts", "testing_*", "wb_region"),
                "Documentation"=c("Administrative level (World Bank regions = -1, countries = 0, states/provinces = 1, metropolitan areas = 1.5, counties = 2)", "Metropolitan area FIPS code", "Total number of confirmed COVID-19 cases", "Doubling rate of confirmed COVID-19 cases (number of days for COVID-19 cases to double)", "Date of first confirmed COVID-19 case", "T if new COVID-19 cases reported, F if none", "Number of new confirmed COVID-19 cases", "Percent increase in confirmed COVID-19 cases", "Total number of confirmed COVID-19 cases per 100,000 persons", "Weekly rolling average of new confirmed COVID-19 cases", "Weekly rolling average of new confirmed COVID-19 cases 14 days prior", "Difference between a weekly rolling average of new confirmed COVID-19 cases and the weekly rolling average of new confirmed COVID-19 cases 14 days prior", "Weekly rolling average of new confirmed COVID-19 cases per 100,000 persons", "Country GDP per capita", "Country ISO3", "Country name", "Total population of country", "Date", "Days since 100 new confirmed cases of COVID-19 reported", "Days since 10 new deaths due to COVID-19 reported", "Days since 50 new deaths due to COVID-19 reported", "Total number of deaths due to COVID-19", "Doubling rate of deaths due to COVID-19 (number of days for deaths due to COVID-19 to double)", "Date of first death due to COVID-19", "T if new deaths due to COVID-19 reported, F if none", "Number of new deaths due to COVID-19", "Percent increase in deaths due to COVID-19", "Total number of deaths due to COVID-19 per 100,000 persons", "Weekly rolling average of new deaths due to COVID-19", "Weekly rolling average of new deaths due to COVID-19 14 days prior", "Difference between a weekly rolling average of new deaths due to COVID-19 and the weekly rolling average of new deaths due to COVID-19 14 days prior", "Weekly rolling average of new deaths due to COVID-19 per 100,000 persons", "Number of days between first confirmed case of COVID-19 and first death due to COVID-19", "Year that GDP was last updated", "GDP per capita", "ISO3 code", "Latitude", "Location code", "Longitude", "T for most recent row of data, F for all others", "Location name", "Number of administrative divisions", "Total population", "Total number of recovered cases of COVID-19", "Doubling rate of recovered cases of COVID-19 (number of days for recovered cases of COVID-19 to double)", "Date of first recovered case of COVID-19", "T if new recovered COVID-19 cases reported, F if none", "Number of new recovered cases of COVID-19", "Percent increase in recovered cases of COVID-19", "Total number of recovered cases of COVID-19 per 100,000 persons", "Weekly rolling average of new recovered cases of COVID-19", "Weekly rolling average of new recovered cases of COVID-19 14 days prior", "Difference between a weekly rolling average of new recovered cases of COVID-19 and the weekly rolling average of recovered cases of COVID-19 14 days prior", "Weekly rolling average of new recovered cases of COVID-19 per 100,000 persons", "State ISO3 code", "State name", "County name, county FIPS code, state name", "Documentation found at <a href='https://covidtracking.com/data-definitions'>The COVID Tracking Project</a>", "World Bank region"),
                stringsAsFactors = FALSE,
                check.names = F)
  return(df)
}

####genome functions#####

getISO3_genomic <- function(locations_to_search){
  loc_url <- "https://api.outbreak.info/genomics/location?"
  locs_of_interest=c()
  locs_not_found=c()
  for (i in locations_to_search){
    scroll.id <- NULL
    location.ids <- paste0("name=", paste(i))
    location.ids <- gsub("&", "%26", location.ids)
    results <- list()
    success <- NULL
    while(is.null(success)){
      dataurl <- paste0(loc_url, location.ids)
      dataurl <- gsub(" ", "+", dataurl)
      dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
      t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
      if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
        stop("Could not connect to API. Check internet connection and try again.")
      }else{
      resp <- fromJSON(dataurl, flatten=TRUE)
      results[[length(results) + 1]] <- resp$results
      scroll.id <- resp$'_scroll_id'
      success <- resp$success
      }
    }
    t <- try(rbind_pages(results), silent=T)
    if("try-error" %in% class(t)){
      error=T
      locs_not_found = c(locs_not_found, i)
    }else{
      hits <- rbind_pages(results)
      df=(hits)
      if (nrow(df)==1){
        locs_of_interest=c(locs_of_interest, df$id)
      }else{
        locs_not_found=c(locs_not_found, i)
      }
    }
  }
  if (length(locs_of_interest)==length(locations_to_search)){
    return(locs_of_interest)
  }
  if (length(locs_of_interest)!=length(locations_to_search)){
    locations=c()
    for (i in locs_not_found){
      if (grepl(" ", i, fixed=TRUE)==T){
        locs=paste0("*",i,"*")
        locs=gsub(" ", "*", locs, fixed=TRUE)
      }else{
        locs=paste0("*",i,"*")
      }
      locations=c(locations, locs)
    }
    for (i in 1:length(locations)){
      scroll.id <- NULL
      location.ids <- paste0("name=", paste(locations[i]))
      location.ids <- gsub("&", "%26", location.ids)
      results <- list()
      success <- NULL
      while(is.null(success)){
        dataurl <- paste0(loc_url, location.ids)
        dataurl <- gsub(" ", "+", dataurl)
        dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
        t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
        if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
          stop("Could not connect to API. Check internet connection and try again.")
        }else{
        resp <- fromJSON(dataurl, flatten=TRUE)
        results[[length(results) + 1]] <- resp$results
        scroll.id <- resp$'_scroll_id'
        success <- resp$success
        }
      }
      t2 <- try(rbind_pages(results), silent=T)
      if("try-error" %in% class(t2)){
        print(paste(locs_not_found[i], "not found. Please check spelling."))
        next
      }else{
        hits <- rbind_pages(results)
        df=(hits)
        df$admin_level[df$admin_level == "-1"] <- "World Bank Region"
        df$admin_level[df$admin_level == "0"] <- "country"
        df$admin_level[df$admin_level == "1"] <- "state/province"
        df$admin_level[df$admin_level == "1.5"] <- "metropolitan area"
        df$admin_level[df$admin_level == "2"] <- "county"
        df$full <- paste0(df$label, " (", df$admin_level, ")")
      }
      for (i in df$full){
        print(i)
        loc_sel <- readline("Is this a location of interest? (Y/N): ")
        if ((loc_sel == "Y")|(loc_sel == "y")){
          locs_of_interest = c(locs_of_interest, df$id[df$full==i])
          break
        }
        if ((loc_sel != "Y")&(loc_sel != "y")&(loc_sel != "N")&(loc_sel != "n")){
          print("Expected input is Y or N")
          print(i)
          loc_sel <- readline("Is this a location of interest? (Y/N): ")
          if ((loc_sel == "Y")|(loc_sel == "y")){
            locs_of_interest = c(locs_of_interest, df$id[df$full==i])
            break
          }
        }
      }
    }
  }
  return(locs_of_interest)
}

getGenomicData <- function(query_url, location=NULL, cumulative=NULL, pangolin_lineage=NULL, mutations=NULL, ndays=NULL, frequency=NULL, subadmin=NULL, other_threshold=NULL, nday_threshold=NULL, other_exclude=NULL){
  genomic_url <- "https://api.outbreak.info/genomics/"
  
  q <- c()
  
  q <- c(q, paste0(query_url), "?")
  
  if(!is.null(location)){
    location <- getISO3_genomic(location)
    q <- c(q, paste0("location_id=", location, "&"))
  }
  if(!is.null(cumulative)){
    if (!is.logical(cumulative)){
      stop("cumulative must be in Boolean format")
    }else{
      q <- c(q, paste0("cumulative=", tolower(cumulative)), "&")
    }
  }
  if(!is.null(subadmin)){
    if (!is.logical(subadmin)){
      stop("subadmin must be in Boolean format")
    }else{
      q <- c(q, paste0("subadmin=", tolower(subadmin)), "&")
    }
  }
  if(!is.null(pangolin_lineage)){
    q <- c(q, paste0("pangolin_lineage=", pangolin_lineage, "&"))
  }
  if(!is.null(mutations)){
    mutations <- paste(mutations, collapse=",")
    q <- c(q, paste0("mutations=", mutations, "&"))
  }
  if(!is.null(ndays)){
    q <- c(q, paste0("ndays=", ndays, "&"))
  }
  if(!is.null(frequency)){
    q <- c(q, paste0("frequency=", frequency, "&"))
  }
  if(!is.null(other_threshold)){
    q <- c(q, paste0("other_threshold=", other_threshold, "&"))
  }
  if(!is.null(nday_threshold)){
    q <- c(q, paste0("nday_threshold=", nday_threshold, "&"))
  }
  if(!is.null(other_exclude)){
    other_exclude <- paste(other_exclude, collapse=",")
    q <- c(q, paste0("other_exclude=", other_exclude, "&"))
  }
  
  q <- paste(q, sep="", collapse = "")
  q <- sub("&$", "", q)
  
  dataurl <- paste0(genomic_url, q)
  
  scroll.id <- NULL
  results <- list()
  success <- NULL
  while(is.null(success)){
    dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
    t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
    if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
      stop("Could not connect to API. Check internet connection and try again.")
    }else{
      resp <- fromJSON(dataurl, flatten=TRUE)
      results[[length(results) + 1]] <- resp$results
      scroll.id <- resp$'_scroll_id'
      success <- resp$success
    }
  }
  if (length(results) > 1){
    hits <- rbind_pages(results)
  }else{
    hits <- data.frame(results)
  }
  if ("date" %in% colnames(hits)){
    hits$date=as.Date(hits$date, "%Y-%m-%d")
    hits <- hits[order(as.Date(hits$date, format = "%Y-%m-%d")),]
  }
  return(hits)
}

dataurl <- "https://api.outbreak.info/genomics/prevalence-by-location?=mutations=S:N501Y&location_id=USA_US-CA"
scroll.id <- NULL
results <- list()
success <- NULL
while(is.null(success)){
  dataurl <- ifelse(is.null(scroll.id), dataurl, paste0(dataurl, "&scroll_id=", scroll.id))
  t <- try(fromJSON(dataurl, flatten=TRUE), silent=T)
  if(grepl("Error in open.connection(con, \"rb\")", t[1], fixed=T)){
    stop("Could not connect to API. Check internet connection and try again.")
  }else{
  resp <- fromJSON(dataurl, flatten=TRUE)
  results[[length(results) + 1]] <- resp$results
  scroll.id <- resp$'_scroll_id'
  success <- resp$success
  }
}
if (length(results) > 1){
  hits <- rbind_pages(results)
}else{
  hits <- data.frame(results)
}
  

getSeqCounts <- function(location=NULL, cumulative=NULL, subadmin=NULL){
  df <- getGenomicData(query_url="sequence-count", location = location, cumulative = cumulative, subadmin = subadmin)
  return(df)
}

getGlobalPrevalence <- function(pangolin_lineage, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="global-prevalence", pangolin_lineage = pangolin_lineage, mutations = mutations, cumulative = cumulative)
  return(df)
}

getPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  return(df)
}

plotPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL, include_title=F){
  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  p <- ggplot(data=df, aes(x=date, y=proportion)) + geom_line() + scale_y_continuous(labels = scales::percent, name="percentage")
  p <- p + geom_ribbon(aes(ymin=proportion_ci_lower, ymax=proportion_ci_upper), alpha=0.2)
  if (include_title == T){
    p <- p + ggtitle(paste0("Prevalence of ", toupper(pangolin_lineage), " in ", stringr::str_to_title(location)))
  }
  return(p)
}

getCumulativeBySubadmin <- function(pangolin_lineage, location=NULL, mutations=NULL, ndays=NULL){
  df <- getGenomicData(query_url="lineage-by-sub-admin-most-recent", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, ndays = ndays)
  return(df)
}

getCollectionDateByLocation <- function(pangolin_lineage, location=NULL, mutations=NULL){
  df <- getGenomicData(query_url="most-recent-collection-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}

getSubmissionDateByLocation <- function(pangolin_lineage, location=NULL, mutations=NULL){
  df <- getGenomicData(query_url="most-recent-submission-date-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations)
  return(df)
}

getLag <- function(location=NULL){
  df <- getGenomicData(query_url="collection-submission", location = location)
  df$date_collected <- as.Date(df$date_collected, "%Y-%m-%d")
  df$date_submitted <- as.Date(df$date_submitted, "%Y-%m-%d")
  df$lag <- df$date_submitted - df$date_collected
  return(df)
}

getMutationDetails <- function(mutations){
  df <- getGenomicData(query_url="mutation-details", mutations = mutations)
  return(df)
}

getMutationAcrossLineage <- function(mutations, location=NULL){
  df <- getGenomicData(query_url="mutations-by-lineage", mutations = mutations, location = location)
  return(df)
}

getMutationsByLineage <- function(pangolin_lineage, frequency=0.8){
  df <- getGenomicData(query_url="lineage-mutations", pangolin_lineage = pangolin_lineage, frequency = frequency)
  return(df)
}

getAllLineagesByLocation <- function(location, other_threshold=0.05, nday_threshold=10, ndays=180, other_exclude=NULL, cumulative=F){
  df <- getGenomicData(query_url="prevalence-by-location-all-lineages", location = location, other_threshold = other_threshold, nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, cumulative = cumulative)
  return(df)
}

plotAllLineagesByLocation <- function(location, other_threshold=0.05, nday_threshold=10, ndays=180, other_exclude=NULL, cumulative=F, include_title = F){
  df <- getGenomicData(query_url="prevalence-by-location-all-lineages", location = location, other_threshold = other_threshold, nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, cumulative = cumulative)
  p <- ggplot(df, aes(x=date, y=prevalence_rolling, group=lineage, fill=lineage)) + geom_area()
  if (include_title == T){
    p <- p + ggtitle(paste0("Lineage prevalence in ", str_to_title(location)))
  }
  return(p)
}
