library(shiny)
library(igraph)
library(visNetwork)
library(DT)
library(readxl)

# Cargar los datos desde el archivo Excel usando la ruta proporcionada
file_path <- "/Users/ricardovalenzuela/Desktop/2024/Varios/Ciclo 1.xlsx"
data <- read_excel(file_path)

# Verificar qué datos están disponibles
str(data)
head(data)

# Extraer nodos y aristas del archivo Excel
nodes <- data.frame(
  id = unique(c(data$Source, data$Target)),  # Crear una lista única de nodos
  label = unique(c(data$Source, data$Target)),  # Etiquetas de los nodos
  stringsAsFactors = FALSE
)

edges <- data.frame(from = data$Source, to = data$Target)

# Calcular el grado del nodo para el tamaño
graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)
nodes$degree <- degree(graph)

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Ciclo 1 (1925-1973)"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Panel de Visualización"),
      checkboxInput("showDegree", "Mostrar tamaño según grado del nodo", TRUE),
      DTOutput("nodeTable")
    ),
    mainPanel(
      visNetworkOutput("network", height = "800px")
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Crear la tabla de nodos
  output$nodeTable <- renderDT({
    datatable(nodes, options = list(pageLength = 5))
  })
  
  # Renderizar el grafo interactivo
  output$network <- renderVisNetwork({
    
    # Ajustar el tamaño de los nodos según el grado si está seleccionado
    if (input$showDegree) {
      nodes$size <- nodes$degree * 2  # Multiplicador para mejor visualización
    } else {
      nodes$size <- 10  # Tamaño fijo si no se utiliza el grado
    }
    
    # Crear el grafo interactivo
    visNetwork(nodes, edges) %>%
      visNodes(scaling = list(min = 10, max = 30)) %>%
      visEdges(arrows = "to") %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

