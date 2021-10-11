#' @title Plot daily prevalence of a lineage by location
#'
#' @description Plots the daily prevalence of a PANGO lineage by location
#'
#' @param pangolin_lineage: (optional) PANGO lineage name or vector of PANGO lineage names. Either `pangolin_lineage` or `mutations` needs to be specified. A list of lineages will return a long dataframe with `lineage` as a variable; if you want to calculate the prevalence of lineage1 or lineage2 together, enter the lineages separated by " OR ". For instance, to calculate the prevalence of Delta, you'll need to supply `"B.1.617.2 OR AY.1 OR AY.2 OR ..."` **Be sure to include the space around "OR" and it must be capitalized.**
#' @param mutations: (optional) a `vector` of mutation(s). Either `pangolin_lineage` or `mutations` needs to be specified.
#' @param location: (optional) a location name
#' @param cumulative: (optional) `Boolean` (T/F), T returns cumulative prevalence since first day of detection
#' @param include_title: `Boolean` (T/F), T returns plot with title, F returns plot without title (default=F)
#'
#'@return ggplot2 object
#'
#'@examples
#'plotPrevalenceByLocation(pangolin_lineage = "P.1", location = "Brazil")
#'plotPrevalenceByLocation(pangolin_lineage = c("B.1.1.7", "B.1.427 OR B.1.429", "B.1.617.2"), location = "United States", labelDictionary = c("B.1.427 OR B.1.429" = "Epsilon"))
#'
#'
#' @export

plotPrevalenceByLocation <- function(pangolin_lineage, location, mutations = NULL, cumulative = NULL, include_title = TRUE, labelDictionary = NULL){
  df <- getPrevalenceByLocation(pangolin_lineage=pangolin_lineage, location=location, mutations=mutations, cumulative=cumulative)
  cat("Plotting data...", "\n")

  if(!is.null(labelDictionary)) {
    df = df %>%
      mutate(lineage = ifelse(is.na(unname(labelDictionary[lineage])), lineage, unname(labelDictionary[lineage])))

  }

  p <- ggplot(df, aes(x = date, y = proportion, fill = lineage, colour = lineage, group = lineage)) +
    geom_ribbon(aes(ymin = proportion_ci_lower, ymax = proportion_ci_upper), alpha = 0.35, size = 0) +
    geom_line() +
    scale_x_date(date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_colour_manual(values = COLORPALETTE[-1]) +
    scale_fill_manual(values = COLORPALETTE[-1]) +
    theme_minimal() +
    theme(legend.position = "bottom", axis.title = element_blank())


  if (include_title == TRUE) {
    if(is.null(location)) {
      location = "the World"
    }

    p <- p + ggtitle(paste0("Prevalence of ", toupper(pangolin_lineage), " in ", stringr::str_to_title(location)))
  }
  return(p)
}
