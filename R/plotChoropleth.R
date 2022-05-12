#' Basic plotting function to create a choropleth (map with areas shaded by prevalence)
#'
#' @param df Dataframe resulting from calling \link[outbreakinfo]{getCumulativeBySubadmin}
#' @param fillVar (optional) Which variable within `df` that should be used to fill the location areas. (`proportion` by default)
#' @param title (optional) Title to include on plot
#' @param subtitle (optional) Subtitle to include on plot
#' @param proj4 (optional) PROJ4 projection string used to project geographic coordinates. \href[Wagner VII]{https://proj.org/operations/projections/wag7.html}, appropriate for World maps, is used by default
#'
#' @import sf
#' @return
#' @export
#'
#' @examples

plotChoropleth = function(df, fillVar = "proportion", title = NULL, subtitle = NULL, proj4 = "+proj=wag7 +lon_0=11 +datum=WGS84 +units=m +no_defs") {
  if(!is.null(df) && nrow(df) > 1){
    # color palette: ColorBrewer's YlGnBu:https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=9
    CHOROPLETH_PALETTE = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
    
    # Reproject as Mollweide projection
    df_projected = st_transform(df, crs = proj4)
    
    df_projected = df_projected %>%
      filter(NAME_0 != "Antarctica")
    
    
    p = ggplot(df_projected, aes_string(fill = fillVar)) +
      geom_sf(size = 0.1, colour = "#555555") +
      scale_fill_gradientn(colours = CHOROPLETH_PALETTE, na.value = "#babab0", labels=scales::percent, limits = c(0,1)) +
      labs(caption="Enabled by data from GISAID (https://gisaid.org/)") +
      theme_void() +
      ggtitle(title, subtitle = subtitle) +
      theme(legend.position = "bottom", plot.caption = element_text(size = 18))
    
    return(p)
  } else {
    warning("No data supplied to the plotting function.")
  }
}