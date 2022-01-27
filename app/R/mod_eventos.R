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

    tabPanel("Eventos",
             
             sidebarLayout(
               
               sidebarPanel(
                 
                 tabsetPanel(
                   
                   tabPanel("Ingresar",
                            
                            uiOutput(ns("et")),
                            
                            uiOutput(ns("salida")),
                 
                            uiOutput(ns("tension")),
                            
                            dateInput(ns("fecha"), "FECHA", language="es",value = Sys.Date()),
                            
                            checkboxInput(ns("ver_hora"), "Ingresar Hora", value = TRUE),
                            conditionalPanel(
                              condition = "input.ver_hora == true", ns = ns,
                              shinyTime::timeInput(ns("hora"), "HORA", value = Sys.time(), seconds = FALSE)
                            ),
                            
                            selectInput(ns("evento"), "TIPO DE EVENTO", choices = c("RECIERRE", "CORTE")),
                            
                            actionButton(ns("ingresar"), "Ingresar Evento"),
                            
                            useShinyalert()
                            
                            ),
                   
                   tabPanel("Modificar",
                            
                            tags$div(class="h4", checked=NA,
                                     tags$p("Ingrese el ID del evento a modificar:"),
                            ),
                            
                            uiOutput(ns("id_modificar")),
                            
                            tags$div(class="h4", checked=NA,
                                     tags$p("Modifique los campos que desee:"),
                            ),
                            
                            uiOutput(ns("et_modificar")),
                            
                            uiOutput(ns("salida_modif")),
                            
                            uiOutput(ns("tension_modif")),
                            
                            uiOutput(ns("fecha_modif")),
                            
                            uiOutput(ns("hora_modif")),
                            
                            uiOutput(ns("evento_modif")),
                            
                            actionButton(ns("modificar_evento"), "Modificar Evento"),
                            
                            useShinyalert()
                     
                   )
                 )
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
    
    ##### INGRESAR #####
    
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
    
    ##### MODIFICAR #####
    
    output$id_modificar <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT id_evento
          FROM eventos
          ORDER BY id_evento ASC;")
      id_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectizeInput(NS(id,"id_modif"), "ID", options=list(maxOptions = 100000), id_choices)
    })
    
    output$et_modificar <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT et
          FROM eett
          ORDER BY et ASC;")
      et_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      res <- dbSendQuery(conn, "
          SELECT et
          FROM salidas
          LEFT JOIN  eett ON
          salidas.id_et = eett.id_et
          LEFT JOIN  eventos ON
          salidas.id_salida = eventos.id_salida
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      et_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"eett_modificar"), "ET", selected = et_selected, choices = et_choices)
    })
    
    output$salida_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT salida
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et
          WHERE et = ?;")
      dbBind(res, list(input$eett_modificar))
      salida_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      res <- dbSendQuery(conn, "
          SELECT salida
          FROM salidas
          LEFT JOIN  eventos ON
          salidas.id_salida = eventos.id_salida
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      salida_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"salida_modificar"), "Ingresar nombre de la salida", 
                  selected = salida_selected, choices = salida_choices)
    })
    
    output$tension_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT tension
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et
          WHERE et = ? AND salida = ?;")
      dbBind(res, list(input$eett_modificar, input$salida_modificar))
      tension_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      res <- dbSendQuery(conn, "
          SELECT tension
          FROM salidas
          LEFT JOIN  eventos ON
          salidas.id_salida = eventos.id_salida
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      tension_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"tension_modificar"), "Ingresar nivel de tensión", 
                  selected = tension_selected, choices = tension_choices)
    })
    
    output$fecha_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT fecha
          FROM eventos
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      fecha_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      dateInput(NS(id,"fecha_modificar"), "Ingresar fecha", language="es", value = fecha_selected)
    })

    output$hora_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT hora
          FROM eventos
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      hora_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      shinyTime::timeInput(NS(id,"hora_modificar"), "Ingresar hora", seconds = FALSE, value = as.POSIXct(hora_selected, format = "%H:%M"))
    })
    
    output$evento_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT evento
          FROM eventos
          WHERE id_evento = ?;")
      dbBind(res, list(input$id_modif))
      evento_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"evento_modificar"), "Ingresar tipo de evento", selected = evento_selected, choices = c("RECIERRE", "CORTE"))
    })
    
    observeEvent(input$modificar_evento, {
      shinyalert(
        title = "Confirmar modificación",
        text = "¿Modificar evento?",
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
                LEFT JOIN  eett ON
                salidas.id_et = eett.id_et
                WHERE et = ? AND salida = ? AND tension = ?;")
            dbBind(res, list(input$eett_modificar, input$salida_modificar, input$tension_modificar))
            id_salida <- dbFetch(res)[1,1]
            dbClearResult(res)
            
            res <- dbSendQuery(conn, "
                UPDATE eventos
                SET id_salida = ?, fecha = ?, hora = ?, evento = ?
                WHERE id_evento = ?;")
            dbBind(res, list(id_salida, as.character(as.Date(input$fecha_modificar)),
                             strftime(input$hora_modificar, format = "%H:%M"),
                             input$evento_modificar,
                             input$id_modif))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "Evento Modificado",
              text = "El evento fue modificado correctamente. Recargar explorador para verlo en la tabla.",
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
