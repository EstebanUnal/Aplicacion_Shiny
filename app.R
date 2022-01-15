#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Estimación para las unidades de vehículos registrados diariamente en el RUNT"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      sliderInput("Vano","Seleccione un año" ,min = 2012 , max = 2018 , value = 2012),
      
      sliderInput("Vmes", "Seleccione un mes (De 1 a 12)", min = 1 , max = 12, value = 1),
      
      sliderInput("Vdia","Seleccione un día del mes (De 1 a 31)", min =1 , max = 31, value = 1),
      
      fileInput('archivo1', 'Seleccione el archivo: Datos_shiny.xlsx',accept=c(".xlsx"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("resumen"),
      tableOutput("prediccion"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$resumen <- renderText({
    
    print("Esta aplicación se utiliza para Estimar las unidades de
          vehículos que se registran diariamente en el RUNT, Se consideran las estimaciones para los
          años 2012 -2018 pues en el 2019 debido a la pandemia el comportamiento
          de los datos puede ser muy diferente al de los datos históricos. \nPor favor colocar 
          en el panel los datos correspodientes a fechas existentes, de lo contrario la aplicación mostrará un error.
          Se debe cargar la base Datos_shiny.xlsx para que la aplicación corra correctamente.")
    
  })
  
  
  
  library(readxl)
  
  output$prediccion <- renderTable({
    
    archivo1 <- input$archivo1
    if(is.null(archivo1))
      return(NULL)
    
    tabla <- read_excel(archivo1$datapath,1)
    
    modelo <- lm(Unidades ~ dia_semana + mes + as.factor(dia_anual) + as.factor(dia_mes) , data = tabla)
    
    Y_predic <- predict(modelo)
    Y_num <- c(1:length(Y_predic))
    
    Y_predic <- ifelse(test = Y_predic < 0, yes = 0 , no = Y_predic)
    
    X <- data.frame(Numero = Y_num , Prediccion = Y_predic)
    
    d1 <- as.character(input$Vdia)
    m1 <- as.character(input$Vmes)
    a1 <- as.character(input$Vano)
    
    fecha <- paste(a1,m1,d1,sep = "-")
    fecha <- as.Date(fecha)
    
    nombre_mes <- format.Date(fecha,'%B')
    nombre_dia_sem <- format.Date(fecha, '%A')
    
    aux_ano <- paste(a1,"01-01",sep = "-")
    aux_ano <- as.Date(aux_ano)
    
    dia_ano <- as.numeric(fecha - aux_ano)
    dia_ano <- dia_ano + 1
    
    datos_nuevos <- data.frame(dia_semana = nombre_dia_sem, mes = nombre_mes , dia_anual = as.factor(dia_ano) ,
                               dia_mes = as.factor(input$Vdia))
    
    datos_predicho <- round(predict(modelo, newdata = datos_nuevos))
    
    datos_predicho <- ifelse(datos_predicho < 0 , yes = 0 , no = datos_predicho)
    
    Y <- data.frame(Dia_semana = nombre_dia_sem ,Fecha = as.character(fecha) ,Estimacion_Unidades_Registradas = datos_predicho)
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)






