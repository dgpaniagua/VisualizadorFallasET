#' eventos UI Function
#'
#' @description Module for second tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
 
mod_eventos_ui <- function(id){
  ns <- NS(id)

    tabPanel("Ingresar Evento",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 uiOutput(ns("et")),
                 
                 uiOutput(ns("salida")),
                 
                 uiOutput(ns("tension")),
                 
                 dateInput(ns("fecha"), "FECHA", value = Sys.Date()),
                 
                 checkboxInput(ns("ver_hora"), "Ingresar Hora", value = TRUE),
                 conditionalPanel(
                   condition = "input.ver_hora == true", ns = ns,
                   shinyTime::timeInput(ns("hora"), "HORA", value = Sys.time(), seconds = FALSE)
                 ),
                 
                 selectInput(ns("evento"), "TIPO DE EVENTO", choices = c("RECIERRE", "CORTE")),
                 
                 actionButton(ns("ingresar"), "Ingresar Evento"),
                 
                 useShinyalert(),
               ),
               mainPanel({
                 DT::dataTableOutput(ns("table"))
               })
             )

  )
}
    
#' eventos Server Functions
#' 
#' @importFrom stats reorder
#'
#' @noRd 
mod_eventos_server <- function(id){
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
    
    output$salida <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT salida
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et
          WHERE et = ?
          ORDER BY salida ASC;")
      dbBind(res, list(input$eett))
      salida_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"sal"), "SALIDA", choices = salida_choices)
    })
    
    output$tension <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT tension
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et
          WHERE et = ? AND salida = ?
          ORDER BY tension ASC;")
      dbBind(res, list(input$eett, input$sal))
      tension_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"volt"), "TENSION", choices = tension_choices)
    })
    
    observeEvent(input$ingresar, {
      shinyalert(
        title = "Confirmar",
        text = "¿Ingresar evento?",
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
              SELECT id_salida
              FROM salidas
              LEFT JOIN eett ON
              salidas.id_et = eett.id_et
              WHERE et = ? AND salida= ? AND tension= ?;")
            dbBind(res, list(input$eett, input$sal, input$volt))
            id_salida <- dbFetch(res)[1,1]
            dbClearResult(res)
            
            res <- dbSendQuery(conn, "
              INSERT INTO eventos(id_salida, fecha, hora, evento)
              VALUES (?, ?, ?, ?);")
            if(input$ver_hora){
              dbBind(res, list(id_salida, as.character(as.Date(input$fecha)), strftime(input$hora, "%H:%M"), input$evento))
            } else {
              dbBind(res, list(id_salida, as.character(as.Date(input$fecha)), "", input$evento))
            }
            
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "Evento Ingresado",
              text = "El evento fue ingresado correctamente. Recargar explorador para verlo en la tabla.",
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
          SELECT id_evento, fecha, hora, et, salida, tension, evento
          FROM eventos
          LEFT JOIN salidas ON
          salidas.id_salida = eventos.id_salida
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et;")
      tabla <- dbFetch(res)
      tabla <- arrange(tabla, desc(.data$fecha), desc(.data$hora), desc(.data$id_evento))
      names(tabla) <- c("ID","Fecha", "Hora", "ET", "Salida", "Tension", "Evento")
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      DT::datatable(tabla, rownames = FALSE)
    })
 
  })
}
    
## To be copied in the UI
# mod_eventos_ui("eventos_ui_1")
    
## To be copied in the server
# mod_eventos_server("eventos_ui_1")
