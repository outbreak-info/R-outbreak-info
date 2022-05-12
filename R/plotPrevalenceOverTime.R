#' @title Plot daily prevalence of a lineage by location
#'
#' @description Plots the daily prevalence of a PANGO lineage by location
#'
#' @param df result of the call to \link[outbreakinfo]{getPrevalence}
#' @param colorVar variable to used to color the line traces. `lineage` by default
#' @param title (optional) Title to add to the plot
#' @param labelDictionary (optional) a named list of values to replace in the plot legend.
#' 
#' @import ggplot2
#'
#' @return ggplot2 object
#'
#'@examples
#'p1_brazil <- getPrevalence(pangolin_lineage = "P.1", location = "Brazil")
#'plotPrevalenceOverTime(p1_brazil)
#'
#'us <- getPrevalence(pangolin_lineage = c("B.1.1.7", "B.1.427 OR B.1.429", "B.1.617.2"), location = "United States")
#'plotPrevalenceOverTime(us, labelDictionary = c("B.1.427 OR B.1.429" = "Epsilon"))
#'
#' # Overlay locations
#' b117_india = getPrevalence(pangolin_lineage = "B.1.1.7", location = "India")
#' b117_us = getPrevalence(pangolin_lineage = "B.1.1.7", location = "United States")
#' b117_uk = getPrevalence(pangolin_lineage = "B.1.1.7", location = "United Kingdom")
#' b117 = dplyr::bind_rows(b117_uk, b117_india, b117_us)
#' plotPrevalenceOverTime(b117, colorVar = "location", title="B.1.1.7 prevalence over time")
#' @export

plotPrevalenceOverTime <- function(df, colorVar = "lineage", title = "Prevalence over time", labelDictionary = NULL) {
  if(!is.null(df) && nrow(df) > 0){
    if(!is.null(labelDictionary)) {
      df = df %>%
        mutate(lineage = ifelse(is.na(unname(labelDictionary[lineage])), lineage, unname(labelDictionary[lineage])))
    }
    
    p <- ggplot(df, aes(x = date, y = proportion, colour = .data[[colorVar]], fill = .data[[colorVar]], group = .data[[colorVar]])) +
      geom_ribbon(aes(ymin = proportion_ci_lower, ymax = proportion_ci_upper), alpha = 0.35, size = 0) +
      geom_line(size = 1.25) +
      scale_x_date(date_labels = "%b %Y", expand = c(0,0)) +
      scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
      scale_colour_manual(values = COLORPALETTE[-1]) +
      scale_fill_manual(values = COLORPALETTE[-1]) +
      theme_minimal() +
      labs(caption="Enabled by data from GISAID (https://gisaid.org/)")
      theme(legend.position = "bottom", axis.title = element_blank(), plot.caption = element_text(size = 18))
    
    
    if (!is.null(title)) {
      p <- p + ggtitle(title)
    }
    return(p)
  } else {
    warning("Dataframe is empty.")
  }
}
