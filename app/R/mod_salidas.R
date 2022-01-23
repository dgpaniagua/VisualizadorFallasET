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

  tabPanel("Salidas",
           
   sidebarLayout(
     
     sidebarPanel(
       
       tabsetPanel(
         
         tabPanel("Ingresar",
                  
                  uiOutput(ns("et")),
                  
                  textInput(ns("salida"), "Ingresar nombre de la salida"),
                  
                  selectInput(ns("tension"), "Ingresar nivel de tensión", choices = c("132kV", "66kV", "33kV", "13,2kV", "11kV")),
                  
                  dateInput(ns("fecha_mant"), "Ingresar fecha del último mantenimiento", language="es", value = Sys.Date()),
                  
                  actionButton(ns("ingresar_salida"), "Ingresar Salida"),
                  
                  useShinyalert()
                  ),
         
         tabPanel("Modificar", 
                  
                  #textOutput(ns("text_modificar1")),
                  
                  tags$div(class="h4", checked=NA,
                           tags$p("Ingrese el ID de la salida a modificar:"),
                  ),
                  
                  uiOutput(ns("id_modificar")),
                  
                  #textOutput(ns("text_modificar2")),
                  
                  tags$div(class="h4", checked=NA,
                           tags$p("Modifique los campos que desee:"),
                  ),
                  
                  uiOutput(ns("et_modificar")),
                  
                  uiOutput(ns("salida_modif")),
                  
                  uiOutput(ns("tension_modif")),
                  
                  uiOutput(ns("fecha_mant_modif")),
                  
                  actionButton(ns("modificar_salida"), "Modificar Salida"),
                  
                  useShinyalert()
                  
                  )
         
       ),
      
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
                INSERT INTO salidas(id_et, salida, tension, fecha_mant)
                VALUES (?, ?, ?, ?);")
            dbBind(res, list(id_et, input$salida, input$tension, as.character(as.Date(input$fecha_mant))))
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
    
    ##### MODIFICAR #####

    output$id_modificar <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT id_salida
          FROM salidas
          ORDER BY id_salida ASC;")
      id_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"id_modif"), "ID", choices = id_choices)
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
          WHERE id_salida = ?;")
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
          WHERE id_salida = ?;")
      dbBind(res, list(input$id_modif))
      salida_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      textInput(NS(id,"salida_modificar"), "Ingresar nombre de la salida", value = salida_selected)
    })
    
    output$tension_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT tension
          FROM salidas
          WHERE id_salida = ?;")
      dbBind(res, list(input$id_modif))
      tension_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"tension_modificar"), "Ingresar nivel de tensión", selected = tension_selected, choices = c("132kV", "66kV", "33kV", "13,2kV", "11kV"))
    })
    
    output$fecha_mant_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT fecha_mant
          FROM salidas
          WHERE id_salida = ?;")
      dbBind(res, list(input$id_modif))
      fecha_mant_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      dateInput(NS(id,"fecha_mant_modificar"), "Ingresar fecha del último mantenimiento", language="es", value = fecha_mant_selected)
    })
        
    observeEvent(input$modificar_salida, {
      shinyalert(
        title = "Confirmar modificación",
        text = "¿Modificar salida?",
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
                UPDATE salidas
                SET id_et = ?, salida = ?, tension = ?, fecha_mant = ?
                WHERE id_salida = ?;")
            dbBind(res, list(id_et, input$salida_modificar, input$tension_modificar, as.character(as.Date(input$fecha_mant_modificar)), input$id_modif))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "Salida Modificada",
              text = "La salida fue modificada correctamente. Recargar explorador para verla en la tabla.",
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
    
    ##### TABLA #####
    
    output$table <- DT::renderDataTable({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT id_salida, et, salida, tension, fecha_mant
          FROM salidas
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et;")
      tabla <- dbFetch(res)
      tabla <- arrange(tabla, .data$et, .data$salida)
      names(tabla) <- c("ID", "ET", "Salida", "Tension", "Fecha Mant")
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
