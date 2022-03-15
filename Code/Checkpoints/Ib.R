# Checkpoint 1:
# En esta aplicacion modificamos la plantilla inicial para cargar nuestros datos y hacer dos figuras
# Cargar las librerias
library(shiny)
library(dplyr) # Para manipulacion de datos
library(ggplot2) # Para las figuras

# Cargar los datos
vac <- STNet::vac

vigilancia <- read.csv("../Datos/Alex/VIGILANCIA.csv") %>%
  mutate(YEAR = as.character(YEAR))

captura <- read.csv("../Datos/CAPTURA_HEMATOFAGO.csv") %>%
  mutate(YEAR = as.character(YEAR))

# Definir Interfaz
ui <- fluidPage(
  # Titulo de la applicacion
  titlePanel("Laboratorio 1"),
  # Barra lateral con un input
  sidebarPanel(
    # Input para motivo de movimiento
    selectInput(inputId = "Mun", label = "Municipios:", 
                choices = unique(vac$NOM_MUN), multiple = T, 
                selected = unique(vac$NOM_MUN))
  ),
  # Outputs
  mainPanel(
    plotOutput("VacMun"), # Figura de serie de tiempo
    # plotOutput("VigMun"),
    # plotOutput("CapMun")# Agregar pie chart aqui
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  x <- reactive({
    p <- vac %>% # base de datos
      filter(NOM_MUN %in% input$Mun) # filtramos los datos
  })
  
  output$VacMun <- renderPlot({
    x() %>% 
      group_by(YEAR) %>% 
      summarise(Vac = sum(VAC_BOV)) %>% 
      ggplot(aes(x = YEAR, y=Vac)) +
      geom_bar(position="dodge", stat="identity", fill = "deepskyblue4") +
      labs(x = "Año", y = "Dosis de vacuna aplicados",
           title = "Aplicación de vacuna antirrábica")
  })
  
  # y <- reactive({
  #   p <- vigilancia %>% # base de datos
  #     filter(NOM_MUN %in% input$Mun) %>%
  #     filter(YEAR %in% c("2007", "2008", "2009", "2010", "2011", "2012"))# filtramos los datos
  # })
  # 
  # output$VigMun <- renderPlot({
  # y() %>% 
  #     group_by(YEAR, RESULTADO) %>% 
  #     summarise(N = n()) %>% 
  #     ggplot(aes(x = YEAR, y=N, fill=RESULTADO)) +
  #     geom_bar(position="dodge", stat="identity")+
  #     theme(legend.position = "top") +
  #     labs(x = "Año", y = "Muestras enviadas al laboratorio",
  #          title = "Vigilancia epidemiológica de rabia paralítica en el suroeste del Estado de México")
  # })
  # 
  # z <- reactive({
  #   p <- captura %>% # base de datos
  #     filter(NOM_MUN %in% input$Mun) %>%
  #     filter(YEAR %in% c("2007", "2008", "2009", "2010", "2011", "2012"))# filtramos los datos
  # })
  # 
  # output$CapMun <- renderPlot({
  #   z() %>% 
  #     group_by(YEAR) %>% 
  #     summarise(N = n()) %>% 
  #     ggplot(aes(x = YEAR, y=N)) +
  #     geom_bar(position="dodge", stat="identity", fill = "red4") +
  #     labs(x = "Año", y = "Eventos de captura de murciélago hematófago",
  #          title = "Control de población de murciélago hematófago")
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)