#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage(s) by location
#'
#' @param pangolin_lineage: (optional) PANGO lineage name or vector of PANGO lineage names. Either `pangolin_lineage` or `mutations` needs to be specified. A list of lineages will return a long dataframe with `lineage` as a variable; if you want to calculate the prevalence of lineage1 or lineage2 together, enter the lineages separated by " OR ". For instance, to calculate the prevalence of Delta, you'll need to supply `"B.1.617.2 OR AY.1 OR AY.2 OR ..."` **Be sure to include the space around "OR" and it must be capitalized.**
#' @param mutations: (optional) a `vector` of mutation(s). Either `pangolin_lineage` or `mutations` needs to be specified.
#' @param location: (optional) a location name
#' @param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#'
#' @return dataframe
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
#' getPrevalenceByLocation(mutations = c("S:E484K"))
#'
#' # B.1.1.7 + S:E484K mutation worldwide
#' getPrevalenceByLocation(pangolin_lineage = "B.1.1.7", mutations = c("S:E484K"))
#' @export



getPrevalenceByLocation <- function(pangolin_lineage=NULL, location=NULL, mutations=NULL, cumulative=NULL){
  if(is.null(pangolin_lineage) && is.null(mutations)) {
    stop("Either `pangolin_lineage` or `mutations` needs to be specified")
  }

  if(is.vector(pangolin_lineage)) {
    lineage_string = paste(pangolin_lineage, collapse = ",")
  } else {
    lineage_string = pangolin_lineage
  }

  df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = lineage_string, location = location, mutations = mutations, cumulative = cumulative)

  df = df %>% rename(lineage = query_key)
  return(df)
}
