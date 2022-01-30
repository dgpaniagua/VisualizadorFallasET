#' eett UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_eett_ui <- function(id){
  ns <- NS(id)

  tabPanel("EETT",
           
           sidebarLayout(
             
             sidebarPanel(
               
               tabsetPanel(
                 
                 tabPanel("Ingresar",
                          
                          tags$div(class="h5", checked=NA,
                                   tags$b("INGRESE LOS SIGUIENTES CAMPOS"),
                          ),
                          
                          textInput(ns("et"), "Nombre de la ET"),
                          
                          selectInput(ns("zona_ingresar"), "Zona", choices = c("NORTE", "SUR")),
                          
                          actionButton(ns("ingresar_et"), "Ingresar ET"),
                          
                          useShinyalert()
                 ),
                 
                 tabPanel("Modificar", 
                          
                          tags$div(class="h5", checked=NA,
                                   tags$b("INGRESE EL ID DE LA ET A MODIFICAR"),
                          ),
                          
                          uiOutput(ns("id_modificar")),
                          
                          tags$div(class="h5", checked=NA,
                                   tags$b("MODIFIQUE LOS CAMPOS QUE DESEE"),
                          ),
                          
                          uiOutput(ns("et_modificar")),
                          
                          uiOutput(ns("zona_modif")),
                          
                          actionButton(ns("modificar_et"), "Modificar ET"),
                          
                          useShinyalert()
                          
                 ),
                 
                 tabPanel("Eliminar", 
                          
                          tags$div(class="h5", checked=NA,
                                   tags$b("INGRESE EL ID DE LA ET A ELIMINAR"),
                          ),
                          
                          uiOutput(ns("id_eliminar")),
                          
                          actionButton(ns("eliminar_et"), "Eliminar ET"),
                          
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
    
#' eett Server Functions
#'
#' @noRd 
mod_eett_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    
    ##### INGRESAR #####
    
    observeEvent(input$ingresar_et, {
      shinyalert(
        title = "Confirmar",
        text = "¿Ingresar ET?",
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
                INSERT INTO eett(et, zona)
                VALUES (?, ?);")
            dbBind(res, list(input$et, input$zona_ingresar))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "ET Ingresada",
              text = "La ET fue ingresada correctamente. Recargar explorador para verla en la tabla.",
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
          SELECT id_et
          FROM eett
          ORDER BY id_et ASC;")
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
          WHERE id_et = ?;")
      dbBind(res, list(input$id_modif))
      et_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      textInput(NS(id,"eett_modificar"), "ET", value = et_selected)
    })
    
    output$zona_modif <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT zona
          FROM eett
          WHERE id_et = ?;")
      dbBind(res, list(input$id_modif))
      zona_selected <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"zona_modificar"), "Zona", selected = zona_selected, choices = c("NORTE", "SUR"))
    })
    
    observeEvent(input$modificar_et, {
      shinyalert(
        title = "Confirmar modificación",
        text = "¿Modificar ET?",
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
                UPDATE eett
                SET et = ?, zona = ?
                WHERE id_et = ?;")
            dbBind(res, list(input$eett_modificar, input$zona_modificar, input$id_modif))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "ET Modificada",
              text = "La ET fue modificada correctamente. Recargar explorador para verla en la tabla.",
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
    
    
    ##### ELIMINAR #####
    
    output$id_eliminar <- renderUI({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db"))
      res <- dbSendQuery(conn, "
          SELECT id_et
          FROM eett
          ORDER BY id_et ASC;")
      id_choices <- dbFetch(res)[,1]
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      selectInput(NS(id,"id_elim"), "ID", choices = id_choices)
    })
    
    observeEvent(input$eliminar_et, {
      shinyalert(
        title = "Confirmar eliminación",
        text = "¿Eliminar ET?",
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
                DELETE FROM eett
                WHERE id_et = ?;")
            dbBind(res, list(input$id_elim))
            dbClearResult(res)
            DBI::dbDisconnect(conn)
            shinyalert(
              title = "ET Eliminada",
              text = "La ET fue eliminada correctamente. Recargar explorador para verla en la tabla.",
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
          SELECT id_et, et, zona
          FROM eett;")
      tabla <- dbFetch(res)
      tabla <- arrange(tabla, .data$et)
      names(tabla) <- c("ID", "ET", "Zona")
      dbClearResult(res)
      DBI::dbDisconnect(conn)
      DT::datatable(tabla, rownames = FALSE)
    })
    
  })
}
    
## To be copied in the UI
# mod_eett_ui("eett_ui_1")
    
## To be copied in the server
# mod_eett_server("eett_ui_1")
