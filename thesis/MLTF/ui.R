library(shiny)
library(shinydashboard)
library(DT)
library(shinyjs)
library(plotly)
library(shinycssloaders)
library(shinythemes)
library(bslib)
library(shinyWidgets)

options(shiny.maxRequestSize=30*1024^2)

shinyUI(
  
  dashboardPage( skin="black",
                 dashboardHeader(title=span(strong("Medium Term Load Forecast"), style = "color: black; font-size: 12px")),
                 dashboardSidebar( 
                   sidebarMenu(
                     id='tabs',
                     menuItem('Dataset',tabName = 'dataset', icon =icon("table"), selected = TRUE),
                     menuItem('Forecast Data', tabName = "forecast", icon =icon("chart-pie")),
                     menuItem('Reports and Analytics', tabName = "analytics", icon =icon("chart-pie"))
                   )
                 ),
                 dashboardBody(
                   
                   tags$head(tags$style(HTML('
                               
                                /* body */
                                .content-wrapper, .right-side {
                                background-color: #FFFFFF;
                                }

                                '))),
                 
                 
                   
                   
                   useShinyjs(),
                   tabItems(
                     tabItem(tabName = "dataset",
                             fluidRow(
                               # column(4, 
                               #        dateRangeInput('dateRange',label = "Select Month Range: ",format = "dd/MM",language="en",startview = "month")
                               # ),
                               column(4, align ="center",
                                      fileInput("file", "Upload the dataset file", accept=c('text/csv', 
                                                                                            'text/comma-separated-values,text/plain', 
                                                                                            '.csv'))
                               ),
                               column(12,
                                      withSpinner(DTOutput(outputId = "output_table"))
                               )
                             )
                     ),
                     tabItem(tabName ="forecast",
                             
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   column(9,
                                          withSpinner(plotlyOutput("forecast_total_monthly"))
                                   ),
                                   column(3,
                                          div(style="margin-top:10%",
                                               uiOutput("total_monthly_energy_metrics")
                                          )
                                    )

                                 ),
                                 
                                 
                              ),
                             
                             div(style = "padding: 14px 0px; margin:0%",
                                fluidRow(
                                   column(9,
                                          withSpinner(plotlyOutput("forecast_monthly_peak"))
                                   ),
                                   column(3,
                                          div( style="margin-top:10%",
                                               uiOutput("peak_power_energy_metrics")
                                          )
                                   )
                                ),
                             )
                     ),
                     tabItem(tabName = "analytics",
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   column(12,
                                          withSpinner(plotlyOutput("time_vs_hourly_load_plot"))
                                   ),
                                  
                                 ),
                                 
                                 
                             ),
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   column(6,
                                          withSpinner(plotlyOutput("total_monthly_energy_line_plot"))
                                   ),
                                   column(6,
                                          withSpinner(plotlyOutput("total_monthly_energy_box_plot"))
                                   ),
                                   
                                 ),
                             ),
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   column(6,
                                          withSpinner(plotlyOutput("peak_power_energy_line_plot"))
                                   ),
                                   column(6,
                                          withSpinner(plotlyOutput("peak_power_energy_energy_box_plot"))
                                   ),
                                   
                                 ),
                             ),
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   column(12,
                                          withSpinner(plotlyOutput("yearly_energy_box_plot1"))
                                   ),
                                   
                                   
                                 ),
                             ),
                             div(style = "padding: 14px 0px; margin:0%",
                                 fluidRow(
                                   
                                   column(12,
                                          withSpinner(plotlyOutput("yearly_energy_box_plot2"))
                                   ),
                                   
                                 ),
                             ),
                     )
                     
                   )
                 )
  )
)