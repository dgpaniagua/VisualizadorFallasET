#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinyTime timeInput
#' @importFrom shinyalert useShinyalert shinyalert
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    navbarPage("EPE",
      mod_visual_ui("visual_ui_1"),
      mod_eventos_ui("eventos_ui_1"),
      mod_salidas_ui("salidas_ui_1"),
      mod_eett_ui("eett_ui_1"),
      mod_ayuda_ui("ayuda_ui_1")
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'VisualizadorFallasET'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

