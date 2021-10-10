#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage(s) by location
#'
#' @param pangolin_lineage: PANGO lineage name or list of PANGO lineage names. A list of lineages will return a long dataframe with
#' @param location: a location name
#' @param mutations: (optional) a `vector` of mutation(s)
#' @param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#'
#' @return dataframe
#'
#' @examples
#' # P.1 in Brazil
#' getPrevalenceByLocation(pangolin_lineage = "P.1", location = "Brazil") %>% head()
#'
#' # AY.4 and B.1.617.2 in Brazil (AY.4 and B.1.617.2 separately)
#' getPrevalenceByLocation(pangolin_lineage = c("AY.4", "B.1.617.2"), location = "Brazil") %>% filter(date == "2021-08-01")
#'
#' # AY.4 OR B.1.617.2 in Brazil (AY.4 and B.1.617.2 combined)
#' getPrevalenceByLocation(pangolin_lineage = "AY.4 OR B.1.617.2", location = "Brazil")%>% filter(date == "2021-08-01")
#'
#' @export



getPrevalenceByLocation <- function(pangolin_lineage, location, mutations=NULL, cumulative=NULL){
  if(is.vector(pangolin_lineage)) {
    lineage_string = paste(pangolin_lineage, collapse = ",")
  } else {
    lineage_string = pangolin_lineage
  }

  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = lineage_string, location = location, mutations = mutations, cumulative = cumulative)

  df = df %>% rename(lineage = query_key)
  return(df)
}
