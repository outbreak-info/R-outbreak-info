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

