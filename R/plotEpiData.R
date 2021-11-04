#' @title Plot COVID-19 data of interest
#'
#' @description Plot a metric of interest using up-to-date COVID-19 data from outbreak.info for location(s) of interest (World Bank region, country, state/province, metropolitan area, county).
#'
#'@param locations a vector or list of location names
#'@param variable metric to plot
#'
#' @return ggplot2 object
#'
#' @import ggplot2
#' @import progress
#'
#' @examples
#' p = plotEpiData("Florida", "confirmed_per_100k")
#' show(p)
#'
#' @export


plotEpiData <- function(locations, variable){
  if (missing(variable)){
    stop("Variable to plot not specified")
  }
  location_codes <- getISO3(locations)
  df <- getEpiData(location_id=location_codes)

  if (!(variable %in% colnames(df))){
    print(paste(variable, "is not a valid API field"))
    return(NULL)
  }

  # Hard-coded settings
  variable_dict = list(confirmed = "Cumulative COVID-19 cases",
                       confirmed_numIncrease = "Daily new COVID-19 cases",
                       confirmed_numIncrease_per_100k = "Daily new COVID-19 cases per 100,000 residents",
                       confirmed_rolling_per_100k = "Daily new COVID-19 cases per 100,000 residents",
                       confirmed_rolling = "Daily new COVID-19 cases",
                       confirmed_per_100k = "Cumulative COVID-19 cases per 100,000 residents",
                       dead = "Cumulative COVID-19 deaths",
                       dead_numIncrease = "Daily new COVID-19 deaths",
                       dead_numIncrease_per_100k = "Daily new COVID-19 deaths per 100,000 residents",
                       dead_rolling_per_100k = "Daily new COVID-19 deaths per 100,000 residents",
                       dead_rolling = "Daily new COVID-19 deaths",
                       dead_per_100k = "Cumulative COVID-19 deaths per 100,000 residents"
                       )
  subtitle_dict = list(confirmed_rolling = "7 day rolling average",
                       confirmed_rolling_per_100k = "7 day rolling average",
                       dead_rolling = "7 day rolling average",
                       dead_rolling_per_100k = "7 day rolling average")


  title = variable_dict[[variable]]
  subtitle = subtitle_dict[[variable]]

  title = ifelse(is.null(title), variable, title)

  colour_palette = c("#507ea3","#f28e2c","#e15759","#76b7b2","#59a14f","#edc949","#b475a3","#ff98a8","#9c755f","#bab0ab","#154d7e","#ba6000","#aa2230","#418d88","#277223","#b7990e","#834874","#828282")

  # plot
  # Plot a bargraph if the values are daily increases
  daily_variables = c("confirmed_numIncrease", "dead_numIncrease")
  if(variable %in% daily_variables) {
    p = ggplot(df, aes(date, get(variable), fill = name, group = name)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colour_palette) +
      theme_minimal() +
      theme(legend.position = "none") +
      facet_wrap(~name )
  } else {
    p = ggplot(df, aes(date, get(variable), color = name, group = name)) +
      geom_line(size = 1.25) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.background = element_rect(fill = "#eeeeec", colour = NA)) +
      scale_colour_manual(values = colour_palette)
  }
  
  nrow = ceiling(length(locations)/3)

  p = p +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = scales::comma) +
    theme(text = element_text(size = 16),
          legend.title = element_blank(),
          axis.title = element_blank()) +
    ggtitle(title, subtitle = subtitle) +
    guides(colour = guide_legend(nrow = nrow))
  return(p)
}

