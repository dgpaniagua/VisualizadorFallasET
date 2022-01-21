#' visual UI Function
#'
#' @description Module for first tab.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_visual_ui <- function(id){
  ns <- NS(id)
  tabPanel("Visualizar Eventos",
           
     sidebarLayout(
       
       sidebarPanel(
         
         selectInput(ns("tension"), "Ingresar nivel de tensión", choices = c("MEDIA TENSION"="33kV|11kV|13,2kV", "ALTA TENSION"="132kV", "33kV", "13,2kV")),
         
         selectInput(ns("zona"), "Ingresar Zona", choices = c("SUR", "NORTE", "PROVINCIA"="NORTE|SUR")),
         
         numericInput(ns("top"), "Cantidad de salidas a mostrar", value = 35, min = 10, max = 500),
         
         #Rango de fechas
         dateRangeInput(ns("daterange"), "Ingresar rango de fechas deseado:",
                        start = "2020-07-01",
                        end   = "2021-12-31",
                        separator = " hasta "),
         
         #Botón de descarga
         downloadButton(ns("report"), "Descargar Informe")
       ),
       
       mainPanel(
         plotOutput(ns("plot"))
       )
     )
  )
}
    
#' visual Server Functions
#'
#' @noRd 
mod_visual_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$plot <- renderPlot({
      conn <- DBI::dbConnect(RSQLite::SQLite(), golem::get_golem_options("db")) #With get_golem_options uses the parameter passed in the run_app function (in golem_opts)
      res <- DBI::dbSendQuery(conn, "
          SELECT id_evento, fecha, hora, et, salida, evento, tension, zona, fecha_mant
          FROM eventos
          LEFT JOIN  salidas ON
          eventos.id_salida = salidas.id_salida
          LEFT JOIN eett ON
          salidas.id_et = eett.id_et
          WHERE date(fecha) > ? AND date(fecha) < ?;")
      DBI::dbBind(res, list(as.character(input$daterange[1]), as.character(input$daterange[2])))
      eventos <- DBI::dbFetch(res)
      DBI::dbClearResult(res)
      DBI::dbDisconnect(conn)
      grafico_fallas(eventos, input$tension, input$zona, input$top)},
      height = reactive(50+input$top * 15))

    output$report <- downloadHandler(
      filename = "Informe.pdf",
      content = function(file) {
        report <- system.file("extdata","informe.Rmd", package = "VisualizadorFallasET")

        # Set up parameters to pass to Rmd document
        params <- list(n = input$daterange, top = input$top, db_path = golem::get_golem_options("db"))

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(report, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
                          )
        }
    )
  })
}
    
## To be copied in the UI
# mod_visual_ui("visual_ui_1")
    
## To be copied in the server
# mod_visual_server("visual_ui_1")
