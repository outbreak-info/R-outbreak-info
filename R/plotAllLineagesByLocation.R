#' @title Plot prevalence of lineages by location
#'
#' @description Plot prevalence of all lineages above a specified frequency over time by location
#'
#'@param location a location name
#'@param other_threshold minimum prevalence threshold below which lineages are accumulated under "Other" (default=0.05)
#'@param nday_threshold minimum number of days in which the prevalence of a lineage must be below other_threshold to be accumulated under "Other" (default=10)
#'@param ndays the number of days before the current date to be used as a window to accumulate lineages under "Other" (default=180)
#'@param other_exclude (optional) lineage(s) that are NOT to be included under "Other" even if the conditions specified by the three thresholds above are met
#'@param cumulative `Boolean` (T/F), T returns cumulative prevalence of lineages (default=F)
#'@param include_title `Boolean` (T/F), T returns plot with title, F returns plot without title (default=F)
#'
#'@import dplyr
#'@import stringr
#'@return ggplot2 object
#'
#'@examples
#'plotAllLineagesByLocation(location = "India", other_threshold=0.03, ndays=60)
#'
#' @export

plotAllLineagesByLocation <- function(location, other_threshold=0.05, nday_threshold=10, ndays=180, other_exclude=NULL, cumulative=FALSE, include_title = TRUE){
  df <- getGenomicData(query_url="prevalence-by-location-all-lineages", location = location, other_threshold = other_threshold, nday_threshold = nday_threshold, ndays = ndays, other_exclude = other_exclude, cumulative = cumulative)

  if(is.null(df))
      return(df)

  # set factors
  df$lineage = factor(df$lineage, levels = unique(c("other", df %>% arrange(desc(prevalence_rolling)) %>% pull(lineage))))
  numLineages = levels(df$lineage) %>% length()
  # set anything beyond the 21 color palette to grey
  if(numLineages > length(COLORPALETTE)){
    COLORPALETTE = c(COLORPALETTE, rep("#bab0ab", numLineages - length(COLORPALETTE)))
  }

  cat("Plotting data...", "\n")
  p <- ggplot(df, aes(x=date, y=prevalence_rolling, group=lineage, fill=lineage)) +
    geom_area(colour = "#555555", size = 0.2) +
    scale_x_date(date_labels = "%b %Y", expand = c(0,0)) +
    scale_y_continuous(labels = scales::percent, expand = c(0,0)) +
    scale_fill_manual(values = COLORPALETTE) +
    theme_minimal() +
    labs(caption="Enabled by data from GISAID (https://gisaid.org/)") +
    theme(legend.position = "bottom", legend.background = element_rect(fill = "#eeeeec", colour = NA),
          panel.grid = element_blank(),
          axis.ticks = element_line(size = 0.5, colour = "#555555"), axis.ticks.length = unit(5, "points"),
          axis.title = element_blank()) +
          theme(legend.position = "bottom", axis.title = element_blank(), plot.caption = element_text(size = 18))

  if (include_title == T){
    p <- p +
      ggtitle(paste0("Lineage prevalence in ", str_to_title(location)))
  }
  return(p)
}
