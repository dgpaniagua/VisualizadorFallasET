#' @name grafico_fallas 
#'
#' @description Creates an horizontal bar plot with the number of events per output in descending order.
#' 
#' @param eventos Data Frame with at least the next columns: et, salida, tension, zona.
#' 
#' @param filtro_v String. Filter for column tension. It can be a regex.
#' 
#' @param filtro_zona String. Filter for column zona. It can be a regex.
#' 
#' @param top Numeric. Number of outputs (bars) to plot.
#'
#' @return NULL.
#' 
#' @export
#' 
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import scales
#'
#' @noRd

grafico_fallas <- function(eventos, filtro_v, filtro_zona='', top=35){
  actuaciones <- eventos %>% mutate(interrupt = paste(.data$et, "-", .data$salida, "-", .data$tension)) %>%
    group_by(.data$interrupt) %>%
    summarize(actuaciones = n(), zona = first(.data$zona), tension = first(.data$tension)) %>%
    arrange(desc(actuaciones))
  
  actuaciones %>% filter(grepl(filtro_v, .data$tension), grepl(filtro_zona, .data$zona)) %>% 
    mutate(interrupt = reorder(.data$interrupt, actuaciones)) %>%
    slice_head(n=top) %>%
    ggplot(aes(.data$interrupt, actuaciones)) + 
    geom_col(width = 0.7, position = position_dodge(0.7), fill="steelblue", color="steelblue") + 
    xlab("Salida") +
    ylab("Cantidad de Fallas") +
    coord_flip() + 
    geom_text(aes(label=actuaciones), hjust=-0.25) +
    theme_minimal() + 
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank())
}