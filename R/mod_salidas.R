#' salidas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_salidas_ui <- function(id){
  ns <- NS(id)

  tabPanel("Ingresar Salida",
           
   sidebarLayout(
     
     sidebarPanel(
       
       uiOutput(ns("et")),
       
       textInput(ns("salida"), "Ingresar nombre de la salida"),
       
       selectInput(ns("tension"), "Ingresar nivel de tensión", choices = c("132kV", "66kV", "33kV", "13,2kV", "11kV")),
       
       actionButton(ns("ingresar_salida"), "Ingresar Salida"),
       
       useShinyalert(),
     ),
     mainPanel({
       DT::dataTableOutput(ns("table"))
     })
     
   )
  )
}
    
#' salidas Server Functions
#' 
#' @importFrom stats reorder
#'
#' @noRd 
mod_salidas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$et <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT et
          FROM eett
          ORDER BY et ASC;")
      et_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"eett"), "ET", choices = et_choices)
    })
    
    observeEvent(input$ingresar_salida, {
      shinyalert(
        title = "Confirmar",
        text = "¿Ingresar salida?",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        html = FALSE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = TRUE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        cancelButtonText = "Cancel",
        timer = 0,
        imageUrl = "",
        animation = TRUE,
        callbackR = function(x) {
          if(x) {
            conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
            res <- dbSendQuery(conn, "
                SELECT id_et
                FROM eett
                WHERE et = ?;")
            dbBind(res, list(input$eett))
            id_et <- dbFetch(res)[1,1]
            dbClearResult(res)
            
            res <- dbSendQuery(conn, "
                INSERT INTO salidas(id_et, salida, tension)
                VALUES (?, ?, ?);")
            dbBind(res, list(id_et, input$salida, input$tension))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "Salida Ingresada",
              text = "La salida fue ingresada correctamente. Recargar explorador para verla en la tabla.",
              size = "xs", 
              closeOnEsc = TRUE,
              closeOnClickOutside = TRUE,
              html = FALSE,
              type = "success",
              showConfirmButton = TRUE,
              showCancelButton = FALSE,
              confirmButtonText = "OK",
              confirmButtonCol = "#AEDEF4",
              timer = 0,
              imageUrl = "",
              animation = TRUE
              
            )}}
      )
    })
    
    output$table <- DT::renderDataTable({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT id_salida, et, salida, tension
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et;")
      tabla <- dbFetch(res)
      tabla <- arrange(tabla, .data$et, .data$salida)
      names(tabla) <- c("ID", "ET", "Salida", "Tension")
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      DT::datatable(tabla, rownames = FALSE)
    })
 
  })
}
    
## To be copied in the UI
# mod_salidas_ui("salidas_ui_1")
    
## To be copied in the server
# mod_salidas_server("salidas_ui_1")
