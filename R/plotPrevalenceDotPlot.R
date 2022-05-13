#' Plots a dot plot of prevalences of a variant in a specific location
#'
#' @param df Dataframe resulting from calling \link[outbreakinfo]{getCumulativeBySubadmin}
#' @param title (optional) Title to include on the plot
#' @param subtitle (optional) Subtitle to include on the plot
#' @param dot_size (optional) Dot plot size (in mm)
#' 
#' @import ggplot2
#'
#' @return
#' @export
#'
#' @examples
#' 
#' # Calculate the proportion of sequences assigned to BA.2 or BA.2.12.1 by U.S. State
#' prev_by_state = getCumulativeBySubadmin(pangolin_lineage = c("BA.2", "BA.2.12.1"), location = "United States", ndays = 90)
#' 
#' # Plot the results
#' plotPrevalenceDotPlot(prev_by_state, "Prevalence over the last 90 days in the United States", subtitle = paste0("As of ", format(Sys.Date(), '%d %B %Y')))

plotPrevalenceDotPlot = function(df, title = NULL, subtitle = NULL, dot_size = 3) {
  if(!is.null(df) && nrow(df) > 0){
    xlim = c(0, max(df$proportion))
    
    # Sort the values, so they're arranged from high to low prevalence
    sorted_locations = df %>% arrange(proportion) %>% pull(name)
    df$name = factor(df$name, sorted_locations)
    
    # color palette: ColorBrewer's YlGnBu:https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=9
    CHOROPLETH_PALETTE = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
    
    
    ggplot(df, aes(x = proportion, y = name, fill = proportion)) + 
      geom_segment(aes(x = proportion_ci_lower, xend = proportion_ci_upper, yend = name), alpha = 0.15, size = 3) + 
      geom_point(size = dot_size, shape = 21) +
      scale_x_continuous(limits = xlim, labels = scales::percent) +
      scale_fill_gradientn(values = xlim, colours = CHOROPLETH_PALETTE) + 
      ggtitle(title, subtitle = subtitle) +
      labs(caption="Enabled by data from GISAID (https://gisaid.org/)") +
      theme_minimal() +
      theme(panel.grid.major.y = element_blank(), 
            panel.grid.minor.y = element_blank(), 
            axis.title.y = element_blank(),
            legend.position = "bottom",
            plot.caption = element_text(size = 18))
  }
}