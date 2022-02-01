#' ayuda UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_ayuda_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("Ayuda",
           
     tags$div(class="h3", checked=NA,
              tags$b("Guía de ayuda y uso de la aplicación"),
              tags$br(""),
     ),
     
     tags$div(class="h4", checked=NA,
              tags$b("Funcionamiento general"),
     ),
     
     tags$div(class="h5", checked=NA,
              tags$p(style="text-align: justify;", "La aplicación Visualizador de Fallas de EETT permite ver un gráfico de barras
                     horizontal donde se muestran la cantidad de fallas que se presentaron en una salida
                     en un rango de fechas seleccionado. Este gráfico toma la información de los eventos
                     que se cargan en la pestaña Eventos"),
              tags$p(style="text-align: justify;", "En el panel izquierdo de la página Visualizar Resultados, se
                     tiene una serie de filtros que se pueden aplicar al gráfico:"),
              tags$ol(
                tags$li("Nivel de tensión"),
                tags$li("Zona"),
                tags$li("Cantidad de salidas a mostrar"),
                tags$li("Rango de fechas")
              ),
              tags$p(style="text-align: justify;", "En el gráfico se contabilizarán solo los eventos correspondientes a los filtros ingresados.
                     El filtro Cantidad de salidas a mostrar agrega o quita salidas al final del gráfico, 
                     siempre manteniendo el orden descendente (la primera será la de mayor cantidad de eventos).
                     En el gráfico, el nombre de las salidas se muestran a la izquierda de las barras horizontales,
                     identificadas de la siguiente forma: ET - NOMBRE DE SALIDA - NIVEL DE TENSIÓN."),
              tags$p(style="text-align: justify;", "Debajo de los filtros, se cuenta con el botón Descargar Informe, que permite descargar
                     un informe en formato pdf con un resumen de seis gráficos con algunas combinaciones de filtros aplicados
                     que resultan de interés: zona sur, zona norte y toda la provincia, presentando dos gráficos para cada caso (Alta y Media tensión).
                     Se debe tener en cuenta que los valores ingresados en el Rango de Fechas y la Cantidad de
                     fallas a mostrar se aplicarán al informe pdf. En caso de ingresar muchas salidas, es probable
                     que el tamaño del gráfico no entre correctamente en la hoja, por lo que se recomienda que
                     este valor no sea mayor a 35 para descargar el informe."),
     ),
     
     tags$div(class="h4", checked=NA,
              tags$b("Cargar nuevos eventos"),
     ),
     
     tags$div(class="h5", checked=NA,
              tags$p(style="text-align: justify;",
                     "Los eventos se cargan desde la página Eventos. En esta pantalla, puede verse una tabla
                     donde se muestran todos los eventos cargados, ordenados del más reciente al más antiguo.
                     Esta tabla permite buscar eventos con la barra ubicada arriba a la derecha (la búsqueda aplica
                     a todas las filas y columnas de la tabla)."),
              tags$p(style="text-align: justify;",
                     "La carga de los eventos se realiza desde el panel izquierdo. Este panel cuenta
                     con tres pestañas: Ingresar, Modificar y Eliminar. Para cargar un evento se debe
                     seleccionar la pestaña Ingresar. Luego, se deben completar los campos que se solicitan
                      y presionar el botón Ingresar Evento. Tras la confirmación, el evento se cargará en la 
                     base de datos y será contabilizado y mostrado en la página Visualizar Resultados."),
              tags$p(style="text-align: justify;",
                     "Debe tenerse en cuenta que sólo se podrán ingresar eventos de Salidas que se encuentren
                     cargadas en la base de datos. Cuando se realiza la carga de un evento, en primer lugar se selecciona una ET de la lista desplegable,
                     donde se mostrarán sólo aquellas que se encuentran cargadas. Luego, se seleccionará una
                     Salida y este en caso se mostrarán sólo las salidas correspondientes a la ET seleccionada.
                     Por último, se debe seleccionar un Nivel de Tensión y, como en el caso anterior, solo se
                     mostrarán los niveles de tensión para salidas con el nombre seleccionado y de la ET
                     seleccionada. Si la Salida o Nivel de Tensión no se encuentran disponibles para seleccionarlo,
                     se deberá cargar una nueva Salida como se explica más adelante en esta guía. Si la ET 
                     deseada no se encontrara disponible, se deberá cargar la ET (también explicado en esta guía)
                     y luego cargar Salidas de esa ET."),
              tags$p(style="text-align: justify;",
                     "Las pestañas Modificar y Eliminar permiten realizar modificaciones en eventos existentes
                     y eliminarlos directamente. Esto es para tener mayor flexibilidad en el caso que se 
                     cometan errores al ingresarlos. Para estas operaciones se requiere el ID del evento, que
                      puede encontrarse en la tabla de eventos que se muestra en la pantalla."),
     ),
     
     tags$div(class="h4", checked=NA,
              tags$b("Cargar nuevas salidas"),
     ),
     
     tags$div(class="h5", checked=NA,
              tags$p(style="text-align: justify;",
                     "La carga de Salidas se realiza desde la página Salidas, que presenta una estructura similar
                     a la página Eventos. En la parte central de la pantalla se encuentra una tabla con todas
                     las salidas cargadas y en la parte izquierda un panel desde donde se realiza la carga.
                     Este panel cuenta con las tres pestañas de Ingresar, Modificar y Eliminar."),
              tags$p(style="text-align: justify;",
                     "Como en el caso de los eventos, la carga se realiza desde la pestaña Ingresar. En primer
                     lugar, se solicita seleccionar una ET. Nuevamente, sólo se podrán seleccionar aquellas ET que
                     se encuentren cargadas en la base de datos. Luego, se debe ingresar el nombre y el nivel 
                     de tensión. Se recomienda ingresar el nombre en mayúsculas para mantener la uniformidad de los datos."),
              tags$p(style="text-align: justify;",
                     "Las pestañas Modificar y Eliminar permiten realizar modificaciones en salidas existentes
                     y eliminarlas directamente. Para estas operaciones se requiere el ID de la salida, que
                      puede encontrarse en la tabla de salidas que se muestra en la pantalla."),
     ),
     
     tags$div(class="h4", checked=NA,
              tags$b("Cargar nuevas estaciones transformadoras (EETT)"),
     ),
     
     tags$div(class="h5", checked=NA,
              tags$p(style="text-align: justify;",
                     "La carga de EETT se realiza desde la página EETT, que presenta una estructura similar
                     a la página Eventos y Salidas. En la parte central de la pantalla se encuentra una tabla con todas
                     las EETT cargadas y en la parte izquierda un panel desde donde se realiza la carga.
                     Este panel cuenta con las tres pestañas de Ingresar, Modificar y Eliminar."),
              tags$p(style="text-align: justify;",
                     "Como en los casos anteriores, la carga se realiza desde la pestaña Ingresar. Los datos que se
                     solicitan para realizar la carga son el nombre de la ET y la zona geográfica en la que se 
                     encuentra (NORTE o SUR). Se recomienda ingresar el nombre en mayúsculas para mantener la 
                     uniformidad de los datos."),
              tags$p(style="text-align: justify;",
                     "Las pestañas Modificar y Eliminar permiten realizar modificaciones en EETT existentes
                     y eliminarlas directamente. Para estas operaciones se requiere el ID de la ET, que
                      puede encontrarse en la tabla de EETT que se muestra en la pantalla."),
     ),

   )
}
    
#' ayuda Server Functions
#'
#' @noRd 
mod_ayuda_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_ayuda_ui("ayuda_ui_1")
    
## To be copied in the server
# mod_ayuda_server("ayuda_ui_1")
