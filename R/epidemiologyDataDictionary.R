#' @title Documentation of Epidemiology API fields
#'
#' @description Documents fields included when extracting data from outbreak.info
#'
#' @return dataframe
#'
#' @examples
#' df = epidemiologyDataDictionary()
#' knitr::kable(df)
#'
#' @export

epidemiologyDataDictionary <- function(){
  df = data.frame("API Field" = c("admin_level", "cbsa", "confirmed", "confirmed_doublingRate", "confirmed_firstDate", "confirmed_newToday", "confirmed_numIncrease", "confirmed_pctIncrease", "confirmed_per_100k", "confirmed_rolling", "confirmed_rolling_14days_ago", "confirmed_rolling_14days_ago_diff", "confirmed_rolling_per_100k", "country_gdp_per_capita", "country_iso3", "country_name", "country_population", "date", "daysSince100Cases", "daysSince10Deaths", "daysSince50Deaths", "dead", "dead_doublingRate", "dead_firstDate", "dead_newToday", "dead_numIncrease", "dead_pctIncrease", "dead_per_100k", "dead_rolling", "dead_rolling_14days_ago", "dead_rolling_14days_ago_diff", "dead_rolling_per_100k", "first_dead-first_confirmed", "gdp_last_updated", "gdp_per_capita", "iso3", "lat", "location_id", "long", "mostRecent", "name", "num_subnational", "population", "state_iso3", "state_name", "sub_parts", "wb_region"),
                "Documentation" = c("Administrative level (World Bank regions = -1, countries = 0, states/provinces = 1, metropolitan areas = 1.5, counties = 2)", "Metropolitan area FIPS code", "Total number of confirmed COVID-19 cases", "Doubling rate of confirmed COVID-19 cases (number of days for COVID-19 cases to double)", "Date of first confirmed COVID-19 case", "T if new COVID-19 cases reported, F if none", "Number of new confirmed COVID-19 cases", "Percent increase in confirmed COVID-19 cases", "Total number of confirmed COVID-19 cases per 100,000 persons", "Weekly rolling average of new confirmed COVID-19 cases", "Weekly rolling average of new confirmed COVID-19 cases 14 days prior", "Difference between a weekly rolling average of new confirmed COVID-19 cases and the weekly rolling average of new confirmed COVID-19 cases 14 days prior", "Weekly rolling average of new confirmed COVID-19 cases per 100,000 persons", "Country GDP per capita", "Country ISO3", "Country name", "Total population of country", "Date", "Days since 100 new confirmed cases of COVID-19 reported", "Days since 10 new deaths due to COVID-19 reported", "Days since 50 new deaths due to COVID-19 reported", "Total number of deaths due to COVID-19", "Doubling rate of deaths due to COVID-19 (number of days for deaths due to COVID-19 to double)", "Date of first death due to COVID-19", "T if new deaths due to COVID-19 reported, F if none", "Number of new deaths due to COVID-19", "Percent increase in deaths due to COVID-19", "Total number of deaths due to COVID-19 per 100,000 persons", "Weekly rolling average of new deaths due to COVID-19", "Weekly rolling average of new deaths due to COVID-19 14 days prior", "Difference between a weekly rolling average of new deaths due to COVID-19 and the weekly rolling average of new deaths due to COVID-19 14 days prior", "Weekly rolling average of new deaths due to COVID-19 per 100,000 persons", "Number of days between first confirmed case of COVID-19 and first death due to COVID-19", "Year that GDP was last updated", "GDP per capita", "ISO3 code", "Latitude", "Location code", "Longitude", "T for most recent row of data, F for all others", "Location name", "Number of administrative divisions", "Total population", "State ISO3 code", "State name", "County name, county FIPS code, state name", "World Bank region"),
                stringsAsFactors = FALSE,
                check.names = F)
  return(df)
}
