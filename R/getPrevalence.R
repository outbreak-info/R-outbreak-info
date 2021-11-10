#' @title Retrieve daily prevalence of a lineage by location
#'
#' @description Retrieves the daily prevalence of a PANGO lineage(s) by location
#'
#' @param pangolin_lineage (optional) PANGO lineage name or vector of PANGO lineage names. Either `pangolin_lineage` or `mutations` needs to be specified. A list of lineages will return a long dataframe with `lineage` as a variable; if you want to calculate the prevalence of lineage1 or lineage2 together, enter the lineages separated by " OR ". For instance, to calculate the prevalence of Delta, you'll need to supply `"B.1.617.2 OR AY.1 OR AY.2 OR ..."` **Be sure to include the space around "OR" and it must be capitalized.**
#' @param mutations (optional) a `vector` of mutation(s). Either `pangolin_lineage` or `mutations` needs to be specified. Mutations should be specified in the format `gene:mutation`, like "S:E484K"
#' @param location (optional) a location name
#' @param cumulative (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#' @param logInfo (optional) `Boolean` (T/F), T logs helper messages during API calls.
#'
#' @return dataframe
#'
#' @import purrr
#'
#' @examples
#' # lineage: P.1 in Brazil
#' p1_brazil = getPrevalence(pangolin_lineage = "P.1", location = "Brazil")
#'
#' # AY.4, AY.34, and B.1.617.2 in Brazil (AY.4, AY.34, and B.1.617.2 separately)
#' delta_and_brazil = getPrevalence(pangolin_lineage = c("AY.4", "AY.34", "B.1.617.2"), location = "Brazil")
#' delta_and_brazil[delta_and_brazil$date == "2021-09-01",]
#'
#' # AY.4 OR B.1.617.2 in Brazil (AY.4, AY.34 and B.1.617.2 combined)
#' delta_or_brazil = getPrevalence(pangolin_lineage = "AY.4 OR AY.34 OR B.1.617.2", location = "Brazil")
#' delta_or_brazil[delta_and_brazil$date == "2021-09-01",]
#'
#' # S:E484K mutation prevalence worldwide
#' se484k = getPrevalence(mutations = c("S:E484K"))
#'
#' # B.1.1.7 + S:E484K mutation worldwide
#' b117_se484k = getPrevalence(pangolin_lineage = "B.1.1.7", mutations = c("S:E484K"))
#' @export



getPrevalence <- function(pangolin_lineage=NULL, location=NULL, mutations=NULL, cumulative=FALSE, logInfo=TRUE){
  if(is.null(pangolin_lineage) && is.null(mutations)) {
    stop("Either `pangolin_lineage` or `mutations` needs to be specified")
  }

  if(length(pangolin_lineage) > 1) {
    df <- map_df(pangolin_lineage, function(lineage) getGenomicData(query_url="prevalence-by-location", pangolin_lineage = lineage, location = location, mutations = mutations, cumulative = cumulative, logInfo = logInfo))
  } else {
    df <- getGenomicData(query_url="prevalence-by-location", pangolin_lineage = pangolin_lineage, location = location, mutations = mutations, cumulative = cumulative, logInfo = logInfo)
  }


  if(!is.null(df) && nrow(df) != 0 && cumulative == FALSE){
      df <- df %>%
          rename(lineage = query_key) %>%
          mutate(location = ifelse(is.null(location), "Worldwide", location))
  }

  if(!is.null(df) && nrow(df) != 0 && cumulative == TRUE){
    df <- df %>%
      rename(lineage = key) %>%
      mutate(location = ifelse(is.null(location), "Worldwide", location))
  }

    return(df)
}
