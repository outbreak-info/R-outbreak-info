library(rgdal)

admn0_shp <- readOGR(dsn = "Downloads/ne_10m_admin_0_countries",
                            "ne_10m_admin_0_countries")

admn0_df=admn0_shp@data

getCountryCode <- function(countryname='countryname'){
  iso3=as.character(droplevels(admn0_df$ADM0_A3[admn0_df$ADMIN==countryname]))
  return(iso3)
}

getCountryCode('India')
