#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import RSQLite
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import scales
#' @import DBI
#' @import rmarkdown
#' @import knitr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  mod_visual_server("visual_ui_1")
  mod_eventos_server("eventos_ui_1")
  mod_salidas_server("salidas_ui_1")
  mod_eett_server("eett_ui_1")
  mod_ayuda_server("ayuda_ui_1")
}
