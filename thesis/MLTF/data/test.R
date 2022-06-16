
library(shiny)
library(plotly)


ui <- fluidPage(
  
  uiOutput("user_input_ui"),
  
  plotlyOutput("plot"),
  
  tableOutput("table")
  
  
  
)

server <- function(input, output, session) {
  
  
  RV = reactiveValues(
    
    df = NULL,
    
    df_old = NULL
    
  )
  
  observe({
    
    df = data.frame(A = 1:10, B = 21:30)
    
    RV$df_old = df
    
  })
  
  observe({
    
    df = req(RV$df_old)
    
    ## removing user defined row from df
    df = df[-req(input$user_input),]
    
    RV$df = df
    
  })
  
  output$user_input_ui = renderUI({
    
    df = req(RV$df_old)
    
    numericInput("user_input", "Enter Row Number", min = 1, max = nrow(df), value = 3)
    
  })
  
  
  output$plot = renderPlotly({
    
    df = req(RV$df)
    
    plot_ly(df, x = ~A, y=~B, type = "scatter", mode = "markers")
    
  })
  
  output$table = renderTable({
    
    df = req(RV$df)
    
  })
  
  
  
}

shinyApp(ui, server)