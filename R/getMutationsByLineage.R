#' @title Retrieve mutations by lineage
#'
#' @description Retrieve all mutations in a specified lineage above a threshold
#'
#'@param pangolin_lineage PANGO lineage name or vector
#'@param frequency a number between 0 and 1 specifying the frequency threshold above which to return mutations (default=0.8)
#' @param logInfo (optional) `Boolean` (T/F), T logs helper messages during API calls.
#'
#' @return dataframe
#'
#' @examples
#' p1 = getMutationsByLineage(pangolin_lineage="P.1", frequency=0.5)
#'
#' @export


getMutationsByLineage <- function(pangolin_lineage, frequency=0.75, logInfo=TRUE){

  if(length(pangolin_lineage) > 1) {
    # Set frequency to 0 and then filter after the fact.
    df <- map_df(pangolin_lineage, function(lineage) getGenomicData(query_url="lineage-mutations", pangolin_lineage = lineage, frequency = 0, logInfo = logInfo))

    if(!is.null(df) && nrow(df) != 0){
      mutations = df %>%
        filter(prevalence >= frequency) %>%
        pull(mutation) %>%
        unique()

      df <- df %>%
        filter(mutation %in% mutations)
    }

  } else {
    df <- getGenomicData(query_url="lineage-mutations", pangolin_lineage = pangolin_lineage, frequency = frequency, logInfo = logInfo)
  }

  return(df)
}
