#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage(s) by location
#'
#' @param pangolin_lineage: (optional) PANGO lineage name or vector of PANGO lineage names. Either `pangolin_lineage` or `mutations` needs to be specified. A list of lineages will return a long dataframe with `lineage` as a variable; if you want to calculate the prevalence of lineage1 or lineage2 together, enter the lineages separated by " OR ". For instance, to calculate the prevalence of Delta, you'll need to supply `"B.1.617.2 OR AY.1 OR AY.2 OR ..."` **Be sure to include the space around "OR" and it must be capitalized.**
#' @param mutations: (optional) a `vector` of mutation(s). Either `pangolin_lineage` or `mutations` needs to be specified. Mutations should be specified in the format `gene:mutation`, like "S:E484K"
#' @param location: (optional) a location name
#' @param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#'
#' @return dataframe
#'
#' @import purrr
#'
#' @examples
#' # lineage: P.1 in Brazil
#' getPrevalenceByLocation(pangolin_lineage = "P.1", location = "Brazil") %>% head()
#'
#' # AY.4 and B.1.617.2 in Brazil (AY.4, AY.34, and B.1.617.2 separately)
#' getPrevalenceByLocation(pangolin_lineage = c("AY.4", "AY.34", "B.1.617.2"), location = "Brazil") %>% filter(date == "2021-09-01")
#'
#' # AY.4 OR B.1.617.2 in Brazil (AY.4, AY.34 and B.1.617.2 combined)
#' getPrevalenceByLocation(pangolin_lineage = "AY.4 OR AY.34 OR B.1.617.2", location = "Brazil") %>% filter(date == "2021-09-01")
#'
#' # S:E484K mutation prevalence worldwide
#' getPrevalenceByLocation(mutations = c("S:E484K")) %>% head()
#'
#' # B.1.1.7 + S:E484K mutation worldwide
#' getPrevalenceByLocation(pangolin_lineage = "B.1.1.7", mutations = c("S:E484K")) %>% head()
#' @export



getPrevalenceByLocation <- function(pangolin_lineage=NULL, location=NULL, mutations=NULL, cumulative=NULL){
  if(is.null(pangolin_lineage) && is.null(mutations)) {
    stop("Either `pangolin_lineage` or `mutations` needs to be specified")
  }

  if(length(pangolin_lineage) > 1) {
    df = map_df(pangolin_lineage, function(lineage) getGenomicData(query_url="prevalence-by-location", pangolin_lineage = lineage, location = "United States", mutations = NULL, cumulative = NULL))
  } else {
    df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative)
  }


  if(!is.null(df)){
      df = df %>% 
      rename(lineage = query_key) %>% 
      mutate(location = ifelse(is.null(location), "Worldwide", location))
  }
  
  return(df)
}
