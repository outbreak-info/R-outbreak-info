#' Documentation of genomics (SARS-CoV-2 variant prevalence) API fields
#' @description Returns a dataframe describing the fields included when making calls to the genomics API.
#'
#' @return dataframe
#' @import dplyr
#'
#' @examples
#' knitr::kable(genomicsDataDictionary())
#' @export

genomicsDataDictionary <- function() {
  dict = tribble(~"API Field", ~Documentation,
                 "date", "Sequence collection date, in YYYY-MM-DD format",
                 "lineage", "Pango lineage",
                 "lineage_count", "Total number of samples on a given day assigned to the Pango lineage",
                 "lineage_count_rolling", "7 day rolling average of samples assigned to the Pango lineage on that day",
                 "prevalence", "Estimated prevalence of the variant on that day",
                 "prevalence_rolling", "7 day rolling average estimate of prevalence of the variant on that day",
                 "proportion", "Estimated prevalence of a variant: lineage_count_rolling / total_count_rolling",
                 "proportion_ci_lower", "95% confidence interval lower bound of estimated prevalence of a variant, calculated using Jeffrey's interval",
                 "proportion_ci_upper", "95% confidence interval upper bound of estimated prevalence of a variant, calculated using Jeffrey's interval",
                 "total_count", "Total number of samples sequenced on a given day",
                 "total_count_rolling", "7 day rolling average of total samples sequenced on that day"
                 )
  return(dict)
}
