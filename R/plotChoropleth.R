
plotChoropleth = function(df, fillVar = "proportion", title = NULL, proj4 = "+proj=wag7 +lon_0=11 +datum=WGS84 +units=m +no_defs") {
  # color palette: ColorBrewer's YlGnBu:https://colorbrewer2.org/#type=sequential&scheme=YlGnBu&n=9
  CHOROPLETH_PALETTE = c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb','#41b6c4','#1d91c0','#225ea8','#253494','#081d58')
  
  # Reproject as Mollweide projection
  df_projected = st_transform(df, crs = proj4)
  
  df_projected = df_projected %>%
    filter(NAME_0 != "Antarctica")

  
  p = ggplot(df_projected, aes_string(fill = fillVar)) +
    geom_sf(size = 0.1, colour = "#555555") +
    scale_fill_gradientn(colours = CHOROPLETH_PALETTE, na.value = "#babab0", labels=scales::percent, limits = c(0,1)) +
    theme_void() +
    theme(legend.position = "bottom") 
  
  if(!is.null(title)){ 
    p = p +
      ggtitle(title)
  }
  return(p)
}