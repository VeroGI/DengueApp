#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(dplyr)
library(plotly)


# Hay casos que no tienen información sobre el departamente de residencia.
# Se crea una función para cargar y filtrar los "SIN DATOS"
load_and_filter_data <- function(file_path) {
  read.csv(file_path, stringsAsFactors = FALSE) %>%
    filter(departamento_residencia != "SIN DATOS")
}

# Cargar los datos filtrados
data <- load_and_filter_data("casos_coord.csv")

# Cargar los datos para hacer el mapa de comunas 
comunas_geojson <- geojson_read("map.geojson", what = 'sp')

# Convertir las fechas
data$fecha_inicio_semana_epidemiologica <- as.Date(data$fecha_inicio_semana_epidemiologica, format = "%Y-%m-%d")

#Comenzamos a construir la App y agregamos estilos CSS personalizados
ui <- fluidPage(
  tags$head(
      tags$style(HTML("
    body, .container-fluid {
      background-color: #a6bddb; 
      color: #1E1E1E;
    }
    h4, .control-label {
      font-family: 'Arial', sans-serif;
      font-size: 14px;
      font-weight: bold;
      color: #1E1E1E;
    }
    #plotContainer {
      height: 12px;
      padding-bottom: 15px; 
    }
    #mapContainer {
        margin-bottom: 20px; 
      }
    
  "))
    ),
    
  titlePanel("Dengue en la Ciudad de Buenos Aires"),
  theme = shinytheme("spacelab"),
  fluidRow(
    column(6, 
           h4("Click para seleccionar comuna/s", class = "control-label"),
           div(id = "mapContainer", leafletOutput("map", height = "300px", width = "100%"))
    ),
    column(6,
           sliderInput("WeekRange", "Seleccionar Rango de Semanas", 
                       min = min(data$fecha_inicio_semana_epidemiologica),
                       max = max(data$fecha_inicio_semana_epidemiologica),
                       value = range(data$fecha_inicio_semana_epidemiologica),
                       timeFormat = "%d-%m-%Y", ticks = FALSE),
           h4("Comunas Seleccionadas"),
           verbatimTextOutput("comunaSeleccionada"),
           h4("Totales en el Período Seleccionado"),
           verbatimTextOutput("totalNotifications"),
           verbatimTextOutput("totalFallecidos")
    )
  ),
  fluidRow(
    column(12,
           div(id = "plotContainer", plotlyOutput("linePlot", height = "400px", width = "100%"))
    )
  )
)

# servidor
server <- function(input, output, session) {
  # Crear el mapa con leaflet
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = comunas_geojson, fillColor = "grey", fillOpacity = 0.4,
                  color = "#E1E3E7", weight = 1,
                  highlightOptions = highlightOptions(
                    color = "grey", weight = 2, bringToFront = TRUE),
                  layerId = ~COMUNAS,  
                  label = lapply(paste("<br><b>Comuna:</b>", comunas_geojson$COMUNAS, "<br><b>Barrios:</b> ", tools::toTitleCase(gsub("-","", tolower(comunas_geojson$BARRIOS)))), htmltools::HTML),
                  ) %>% 
  
      setView(lng = -58.45, lat = -34.62, zoom = 11) 
  })
  
  # Variable reactiva para almacenar las comunas seleccionadas del mapa
  comunasSeleccionadas <- reactiveVal(character(0))
  
  # Actualizar las comunas seleccionadas cuando se hace clic en el mapa
  observeEvent(input$map_shape_click, {
    comuna <- input$map_shape_click$id
    if (comuna %in% comunasSeleccionadas()) {
      comunasSeleccionadas(setdiff(comunasSeleccionadas(), comuna))
    } else {
      comunasSeleccionadas(c(comunasSeleccionadas(), comuna))
    }
  })
  
  # Mostrar las comunas seleccionadas
  output$comunaSeleccionada <- renderText({
    if (length(comunasSeleccionadas()) == 0) {
      "Comunas seleccionadas: Toda la Ciudad"
    } else {
      paste("Comunas seleccionadas:", paste(comunasSeleccionadas(), collapse = ", "))
    }
  })
  
  # Datos filtrados según las comunas seleccionadas y el rango de semanas
  filteredData <- reactive({
    req(input$WeekRange)
    filtered <- data %>%
      filter(fecha_inicio_semana_epidemiologica >= input$WeekRange[1] & 
               fecha_inicio_semana_epidemiologica <= input$WeekRange[2])
    
    if (length(comunasSeleccionadas()) > 0) {
      # Mapeo para obtener el nombre de la comuna desde el número seleccionado en el mapa
      nombres_comunas <- paste("COMUNA", comunasSeleccionadas())
      filtered <- filtered %>% filter(departamento_residencia %in% nombres_comunas)
    }
    
    filtered
  })
  
  filteredSumData <- reactive({
    req(filteredData())
    filteredData() %>%
      group_by(fecha_inicio_semana_epidemiologica) %>%
      summarise(total_notificaciones = sum(n_notificaciones, na.rm = TRUE),
                total_fallecidos = sum(n_fallecidos_confirmados_por_laboratorio, na.rm = TRUE))
  })
  
  output$linePlot <- renderPlotly({
    req(filteredSumData())
    if (nrow(filteredSumData()) == 0) return(NULL)
    
    plot_ly(filteredSumData(), x = ~fecha_inicio_semana_epidemiologica, y = ~total_notificaciones, type = 'scatter', mode = 'lines+markers',
            text = ~paste("Fecha: ", format(fecha_inicio_semana_epidemiologica, "%d-%m-%Y"), "<br>Casos: ", total_notificaciones),
            hoverinfo = 'text') %>%
      layout(
        title = "Notificaciones en comunas y período seleccionados",
        xaxis = list(title = "Semana Epidemiológica"),
        yaxis = list(title = "Cantidad de Notificaciones"),
        hoverlabel = list(bgcolor = "white", font = list(size = 12)),
        margin = list(t = 60)
      )
  })
  
  # Mostrar el total de notificaciones en el rango seleccionado
  output$totalNotifications <- renderText({
    req(filteredSumData())
    total_notificaciones <- sum(filteredSumData()$total_notificaciones)
    paste(total_notificaciones, "notificaciones")
  })
  
  # Mostrar el total de fallecidos en el rango seleccionado
  output$totalFallecidos <- renderText({
    req(filteredSumData())
    total_fallecidos <- sum(filteredSumData()$total_fallecidos)
    paste(total_fallecidos, "fallecidos")
  })
  
  # Actualizar los polígonos del mapa para colorear las comunas seleccionadas de azul
  observe({
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = comunas_geojson,
                  fillColor = ifelse(comunas_geojson$COMUNAS %in% comunasSeleccionadas(), "blue", "grey"),
                  fillOpacity = 0.4,
                  color = "#000000",
                  weight = 1,
                  highlightOptions = highlightOptions(
                    color = "#A2DBB5",
                    weight = 6,
                    bringToFront = TRUE
                  ),
                  layerId = ~COMUNAS,
                  label = lapply(paste("<br><b>Comuna:</b>", comunas_geojson$COMUNAS, "<br><b>Barrios:</b> ", tools::toTitleCase(gsub("-","", tolower(comunas_geojson$BARRIOS)))), htmltools::HTML),
                  
                  )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)