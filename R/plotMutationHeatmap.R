#' Title Compare mutation prevalences between lineages
#' 
#' @description Plots a heatamp of mutation prevalence across particular lineages.
#' @param df Resulting dataframe from calling \link[outbreakinfo]{getMutationsByLineage}
#' @param gene2Plot (optional) string containing which genes to include, e.g. "Orf1a" By default, will limit the mutations to those in the S-gene.
#' @param title (optional) title to add to the plot
#' @param lightBorders (optional) boolean; whether the borders between grid items should be separated with a light or dark border.
#'
#' @import stringr
#' @import tidyr
#' @return
#' @export
#'
#' @examples
#' p1 = getMutationsByLineage(pangolin_lineage = "P.1", frequency = 0.5)
#' plotMutationHeatmap(p1, gene2Plot = "ORF1a")
#'

plotMutationHeatmap = function(df, gene2Plot = "S", title = NULL, lightBorders = TRUE) {
  MUTATIONPALETTE = c('#fff7f3','#fde0dd','#fcc5c0','#fa9fb5','#f768a1','#dd3497','#ae017e','#7a0177','#49006a')
  borderColour = ifelse(lightBorders, "#FFFFFF", "#555555")
  
  # Filter down to one gene
  if(!is.null(df) && nrow(df) != 0){
    df = df %>% filter(gene == gene2Plot)
  }
  
  if(!is.null(df) && nrow(df) != 0){
    df = df %>%
      rowwise() %>%
      mutate(mutation_simplified = toupper(str_split(mutation, ":")[[1]][2])) %>%
      arrange(codon_num)
    
    # create empty grid
    mutation_simplified = df %>% pull(mutation_simplified) %>% unique()
    lineage = df %>% pull(lineage) %>% unique()
    blank = crossing(lineage, mutation_simplified)
    
    # refactor the mutations to sort them
    blank$mutation_simplified = factor(blank$mutation_simplified, levels = mutation_simplified)
    df$mutation_simplified = factor(df$mutation_simplified, levels = mutation_simplified)
    
    p = ggplot(df, aes(x = mutation_simplified, y = lineage, fill = prevalence)) +
      geom_tile(colour = borderColour, fill = "#dedede", data = blank) +
      geom_tile(colour = borderColour) +
      theme_minimal() +
      coord_fixed() +
      xlab("mutation") +
      scale_fill_gradientn(colours = MUTATIONPALETTE, limits = c(0,1), labels = scales::percent) +
      labs(caption="Enabled by data from GISAID (https://gisaid.org/)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.9, hjust=1),
            panel.grid = element_blank(),
            legend.position = "bottom",
            plot.caption = element_text(size = 18)
      )
    
    if(!is.null(title)) {
      p = p + ggtitle(title)
    }
    return(p)
  } else {
    warning("No data found. Check if there are mutations present in the gene you specified.")
  }
}
