

## Packages
{
  ## To create shinydashboard
  library(shinydashboard)
  
  ## To interact with google drive
  ## https://googledrive.tidyverse.org/
  ## https://cran.r-project.org/web/packages/googledrive/googledrive.pdf
  library(googledrive)
  
  ## To create non authorization for communication with google drive
  library(gargle)
  
  ## Create interactive plots
  library(plotly)
  
  ## Create spinning wheels
  library(shinycssloaders)
  
  ## Create awesome buttons
  library(shinyWidgets)
  
  ## Get weather data
  library(darksky)
  
  ## Run neural network
  library(neuralnet)
  
  ## Read and write .xlsx files
  library(openxlsx)
  
  ## Create awesome data tables
  library(DT)
  
  ## USe java script to hide/show UI objects
  library(shinyjs)
  
  ## This package has accuracy() to calculate error metrics
  library(forecast)
  
  ## Manipulate Data
  library(reshape2)
  
  ## New Installation
  
  ## Page loading screens
  library(shinybusy)
  
  library(dplyr)
  
  library(data.table)
  
  library(dummies)
  
  library(RColorBrewer)
  
  library(hydroTSM)
  
  library(xts)
  
  library(zoo)
  
  library(stargazer)
}


## To set the time zone to Pakistan
Sys.setenv(TZ="Asia/Karachi")


testing = FALSE

upload_to_googledrive = FALSE


## This sets pathway to exchange data with google drive. The .secrets folder is very importnat
## Read this https://gargle.r-lib.org/articles/non-interactive-auth.html
if(testing){
  
  options(gargle_oauth_cache = ".secrets", gargle_oauth_email = TRUE)
  
} else {
  
  options(gargle_oauth_cache = ".secrets2", gargle_oauth_email = TRUE)
}




round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}



check_all_cloumn_exist = function(weather_data_hourly){
  library(tibble)
  
  if (is.null( weather_data_hourly )){
    df = data.frame(
      time = rep(NA, 24) ,
      summary = rep(NA, 24) ,
      icon = rep(NA, 24) ,
      precipIntensity = rep(NA, 24) ,
      precipProbability = rep(NA, 24) ,
      temperature =rep(NA, 24) ,
      apparentTemperature =rep(NA, 24) ,
      dewPoint =rep(NA, 24) ,
      humidity =rep(NA, 24) ,
      pressure = rep(NA, 24) ,
      windSpeed = rep(NA, 24) ,
      windGust =rep(NA, 24) 
    )
    weather_data_hourly = df
  }
  
  else
  {
    cols =  c("time","summary","icon","precipIntensity","precipProbability","temperature","apparentTemperature" ,"dewPoint","humidity","pressure","windSpeed","windGust")
    for (i in  (0: length(cols) ) ){
      
      col_name <- cols[i]
      add <-col_name[!col_name%in%names(weather_data_hourly)] 
      if(length(add)!=0) 
        weather_data_hourly[add] <- rep(NA ,24)
    }
  }
  return (weather_data_hourly)
} 

get_weather_data = function(start_date,latitude,longitude){
  
  ## Log
  cat("Fetching Weather Data for Forecast \n")
  
  ## Set API key
  Sys.setenv("DARKSKY_API_KEY" = "0f45093404bb6b28d8765d4229b43732")
  
  ## 0f45093404bb6b28d8765d4229b43732
  ## 9ab8041467f5d99ccc54828d67cc556b
  ## a8067a61e49ff99d860e66bd3e534c0f
  ## Set start date
  start_date = as.Date(start_date)
  
  ## Get weather data
  weather_data_hourly= get_forecast_for(latitude,longitude,
                                        start_date, units="si")$hourly
  
  
  weather_data_hourly = check_all_cloumn_exist(weather_data_hourly)
  
  ## Change column names
  cols =  c("time","summary","icon","precipIntensity","precipProbability","temperature","apparentTemperature" ,"dewPoint","humidity","pressure","windSpeed","windGust")
  weather_data_hourly =  subset ( weather_data_hourly , select = cols )
  colnames(weather_data_hourly) [1] = "Time"
  return(weather_data_hourly)
}



## Add Season Label
get_season_data = function(start_date,end_date){
  
  # start_date = as.POSIXct("2020-08-12 PKT")
  # end_date = as.POSIXct("2020-12-23 23:00:00 PKT")
  
  # start_date = as.POSIXct("2020-12-26 PKT")
  # end_date = as.POSIXct("2020-12-26 23:00:00 PKT")
  
  Season_df = data.frame(
    Time = seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour"),
    Month = month( as.POSIXlt(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour")) )
  )
  
  #convert month to Seasons
  Season_df$Month [ Season_df$Month == 12 | Season_df$Month == 1 | Season_df$Month == 2 ] <- "Winter"
  Season_df$Month [ Season_df$Month == 3 | Season_df$Month == 4 | Season_df$Month == 5 ] <- "Spring"
  Season_df$Month [ Season_df$Month == 6 | Season_df$Month == 7 | Season_df$Month == 8 | Season_df$Month == 9 ] <- "Summer"
  Season_df$Month [ Season_df$Month == 10 | Season_df$Month == 11] <- "Autumn"
  
  
  # temp = factor(Season_df$Month, levels = c("Winter", "Spring", "Summer", "Autumn"))
  # 
  # temp = dummy(temp, sep = ".", drop = FALSE)
  # Season_df = cbind(data.frame(Time = Season_df[,1]), as.data.frame(temp, col.names = colnames(temp)))
  
  colnames(Season_df)[2] = c("Season")
  
  return (Season_df)
}


## Add Weekday Label
get_week_day_data = function(start_date, end_date){
  
  
  #   start_date = as.POSIXct("2019-12-12 PKT")
  #   end_date = as.POSIXct("2020-12-23 23:00:00 PKT")
  
  # start_date = as.POSIXct("2020-12-25 PKT")
  # end_date = as.POSIXct("2020-12-25 23:00:00 PKT")
  
  # Week_df = data.frame(
  #   Time = seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour"),
  #   Weekday = as.POSIXlt(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour"))$wday+1
  # )
  # 
  # temp = factor(Week_df$Weekday, levels = 1:7)
  # 
  # temp = dummy(temp, sep = ".", drop = FALSE)
  # temp = as.data.frame(temp, col.names = colnames(temp))
  # Week_df = cbind(data.frame(Time = Week_df[,1]), temp)
  
  #Sunday =1, Monday=2, Tuesday =3, Wednesday =4 , Thursday =5, Friday = 6, Saturday=7
  
  
  # temp = factor(Season_df$Month, levels = c("Winter", "Spring", "Summer", "Autumn"))
  # 
  # temp = dummy(temp, sep = ".", drop = FALSE)
  # Season_df = cbind(data.frame(Time = Season_df[,1]), as.data.frame(temp, col.names = colnames(temp)))
  
  
  # start_date = as.POSIXct("2021-05-14 PKT")
  # end_date = as.POSIXct("2021-05-14 23:00:00 PKT")
  
  
  
  
  
  
  
  
  Week_df = data.frame(
    Time = seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour"),
    Weekday = format(seq(from = as.POSIXct(start_date), to = as.POSIXct(end_date), by="hour"), "%A")
  )
  
  Week_df$Weekday = factor(Week_df$Weekday, levels = c("Sunday", "Monday", "Tuesday", 
                                                       "Wednesday", "Thursday", 
                                                       "Friday", "Saturday"))
  
  day_df = dummy.data.frame(Week_df, names = "Weekday", drop = FALSE)
  
  names(day_df)[-1] = gsub("Weekday", "Time.", names(day_df)[-1])
  
  return(day_df)
  
  # return(Week_df)
  
} 



upload_files_to_google_drive = function(filename, data){
  
  
  ## Save the data locally
  write.csv(data, filename, row.names = FALSE)
  
  if(upload_to_googledrive){
    
    ## Save the data in drive
    drive_upload(filename, filename, overwrite = TRUE)
    
  }
}


download_files_from_google_drive = function(filename, fresh_download = FALSE){
  
  # filename = "DISCO_Load_Maintenance.csv"
  # dir.create("Drive_Download", showWarnings = FALSE)
  
  
  if(fresh_download){file.remove(filename)}
  
  
  if(file.exists(filename)){
    
    ## Read file locally    
    data_df = read.csv(paste0(filename))
    
    ## Convert time to posixct
    data_df$Time = as.POSIXct(data_df$Time)
    
    ## This is to make sure that no time row is missing
    All_time = data.frame(Time = seq.POSIXt(from = data_df$Time[1],
                                            to = data_df$Time[nrow(data_df)],
                                            by = "hour"))
    
    ## This will add NA to rows for which no data is available
    data_df = merge(data_df, All_time, by = "Time", all.y = TRUE)
    
    return(data_df)
    
    
  } else {
    
    
    file_available_on_drive = tryCatch({
      
      ## Save file in Drive_Download folder
      drive_download(filename, path = paste0("Drive_Download/", filename)  , overwrite = TRUE)
      
      TRUE
      
    }, error=function(e) FALSE)  
    
    
    if(file_available_on_drive){
      
      data_df = tryCatch(read.csv(paste0("Drive_Download/", filename)), error=function(e) NULL)
      
      ##Check if downloaded file is null
      if(!is.null(data_df)){
        
        ## Read file locally    
        # data_df = read.csv(paste0("Drive_Download/", filename))
        
        ## Save the data locally
        write.csv(data_df, filename, row.names = FALSE)
        
        ## Convert time to posixct
        data_df$Time = as.POSIXct(data_df$Time)
        
        ## This is to make sure that no time row is missing
        All_time = data.frame(Time = seq.POSIXt(from = data_df$Time[1],
                                                to = data_df$Time[nrow(data_df)],
                                                by = "hour"))
        
        ## This will add NA to rows for which no data is available
        data_df = merge(data_df, All_time, by = "Time", all.y = TRUE)
        
        return(data_df)
        
      } else {
        
        ## Save the data in drive
        drive_upload(filename, filename, overwrite = TRUE)
        
        download_files_from_google_drive(filename)
        
        
      }
      
    } else {
      
      
      return(NULL)
      
    }
    
  }
  
}


## Save log entry
log_entry = function(Username, Entry){
  
  # Username = "admin"
  # Entry = "Test"
  
  loading_screen("Please Wait")
  
  if(!file.exists("Log.csv")){
    
    ## Download Log.csv from drive, it might have the lattest data
    drive_download("Log.csv", overwrite = TRUE)
    
  }
  
  
  ## Again read the Log.csv locally
  Log = read.csv("Log.csv")
  
  ## Change Time data type
  Log$Time = as.POSIXct(Log$Time)
  
  ## Add new entry
  Log = rbind(data.frame(Time = Sys.time(), Username, Entry), Log)
  
  ## Save the new Log locally
  write.csv(Log, "Log.csv", row.names = FALSE)
  
  if(upload_to_googledrive){
    
    ## Save the new Log in drive
    drive_upload("Log.csv", "Log.csv", overwrite = TRUE)
    
  }
  
  # remove it when done
  remove_modal_gif()
  
}


loading_screen = function(onscreen_message){
  
  # show the modal window
  show_modal_gif(
    src = "loading.gif",
    width = "100px", height = "100px",
    modal_size = "s",
    text = onscreen_message
  )
  
}



## App UI
{
  ui <- dashboardPage( 
    
    dashboardHeader(title = "STLF Tool by EIG"),
    
    dashboardSidebar(
      
      sidebarMenuOutput("menu")
      
    ),
    
    dashboardBody(
      
      useShinyjs(),
      
      tabItems(
        
        ## Home tab
        {
          tabItem(tabName = "home",
                  
                  fluidRow(
                    
                    box(width = 12, title = p(strong("Forecast"),style = "font-size:30px"),
                        
                        tabsetPanel(type = "tabs", id = "Home_tab",
                                    
                                    ## NPCC Tab ##
                                    {
                                      tabPanel("NPCC", value = 1,
                                               
                                               
                                               htmlOutput("forecast_info"),
                                               
                                               column(width = 4, 
                                                      div(uiOutput("date_range_home_UI"), align = "left")),
                                               
                                               column(width = 4),
                                               
                                               column(width = 4, br(), div(id = "download_forecast_Data_div", 
                                                                           downloadBttn("download_forecast_Data", "Download", 
                                                                                        size = "sm", color = "default", style = "unite"), 
                                                                           align = "right")),
                                               
                                               
                                               br(),
                                               br(),
                                               br(),
                                               br(),
                                               
                                               withSpinner(plotlyOutput("forecast_plot")),
                                               
                                               tabsetPanel(
                                                 
                                                 tabPanel("Weather Plot", 
                                                          withSpinner(plotlyOutput("cities_weather_plot"))),
                                                 
                                                 tabPanel("Forecast Data", withSpinner(DTOutput("forecast_data_table_UI")))
                                                 
                                               )
                                               
                                      )
                                      
                                    },
                                    
                                    ## DISCO Tab ##
                                    {
                                      tabPanel("DISCOs", value = 2,
                                               
                                               br(),
                                               
                                               column(width = 6,
                                                      htmlOutput("DISCO_forecast_info"),
                                                      
                                                      column(width = 6,
                                                             div(uiOutput("DISCO_date_range_home_UI"), align = "left"))
                                                      
                                               ),
                                               
                                               
                                               
                                               column(width = 6, uiOutput("DISCO_tab_download_selection_UI"),
                                                      
                                                      div(id = "DISCO_download_forecast_Data_div", 
                                                          downloadBttn("DISCO_download_forecast_Data", "Download", 
                                                                       size = "sm", color = "default", style = "unite"), align = "left")),
                                               
                                               br(),
                                               
                                               
                                               column(width = 12, 
                                                      
                                                      br(),br(),br(),br(),
                                                      
                                                      column(width = 4, uiOutput("DISCO_home_tab_DISCO_selection_UI")),
                                                      
                                                      br(),br(),br(),br(),br(),
                                                      
                                                      
                                                      uiOutput("Home_DISCO_tabs"))
                                      )
                                      
                                      
                                    }
                                    
                        )
                    )
                  )
          )
          
        },
        
        # Upload new data tab
        {
          tabItem(tabName = "upload_new_data",
                  
                  fluidRow(
                    
                    box(width = 12,
                        
                        tabsetPanel(type = "tabs", id = "upload_tab",
                                    
                                    ## NPCC Tab ##
                                    {
                                      tabPanel("NPCC", value = 1,
                                               
                                               br(),
                                               
                                               div(htmlOutput("data_required_date_message"), align = "center"),
                                               
                                               
                                               column(width = 3, HTML("<h4><b>Sample File</b></h4>"),
                                                      
                                                      uiOutput('start_date_UI_sample_file'),
                                                      
                                                      uiOutput("end_date_UI_sample_file"),
                                                      
                                                      column(width = 4),
                                                      column(width = 8,downloadBttn("download_sample_file",
                                                                                    label = "Download",
                                                                                    style = "unite",
                                                                                    color = "default",
                                                                                    size = "sm")
                                                      )
                                               ),
                                               
                                               column(width = 9, HTML("<h4><b>Upload New Data</b></h4>"),
                                                      
                                                      div(actionBttn("save_new_data_sample_file", "Save", size = "sm", color = "default", 
                                                                     style = "unite"), align = "right"),
                                                      
                                                      fileInput("new_data_input_sample_file", "Choose .xlsx File",
                                                                width = '50%',
                                                                multiple = FALSE,
                                                                accept = c(".xlsx")),
                                                      
                                                      br(),
                                                      
                                                      
                                                      
                                                      tabsetPanel(
                                                        
                                                        
                                                        
                                                        tabPanel("Uploaded Data", 
                                                                 br(),
                                                                 withSpinner(dataTableOutput("Uploaded_data_table"))),
                                                        
                                                        tabPanel("New Data Uploaded",
                                                                 br(),
                                                                 dataTableOutput("new_data_table_sample_file"))
                                                        
                                                      )
                                                      
                                               )
                                      )},
                                    
                                    ## DISCO Tab ##
                                    {
                                      tabPanel("DISCO", value = 2,
                                               
                                               br(),
                                               
                                               column(width = 3,
                                                      
                                                      HTML("<h4><b>Select Date Range to Enter Data</b></h4>"),
                                                      
                                                      uiOutput('DISCO_date_range_upload_UI'),
                                                      
                                               ),
                                               
                                               column(width = 9,
                                                      
                                                      HTML("<h4><b>Please Upload Daily Maintenance Schedule Translated in MW</b></h4>"),
                                                      
                                                      div(actionBttn("DISCO_save_new_data_sample_file", "Save", size = "sm", color = "default", 
                                                                     style = "unite"), align = "right"),
                                                      
                                                      HTML("<h5><b>Double Click Any Cell Below to Edit its Value</b></h5>"),
                                                      HTML("<h5><b>Click Save Button After Completion</b></h5>"),
                                                      
                                                      br(),
                                                      
                                                      withSpinner(DTOutput('DISCO_Uploaded_data_table')))
                                      )
                                      
                                    }
                                    
                        )
                    )
                  )
          )
        },
        
        # Historic data tab
        {
          tabItem(tabName = "historic_data",
                  
                  
                  fluidRow(
                    
                    box(width = 12, 
                        
                        tabsetPanel(type = "tabs", id = "DISCO_historic_tab",
                                    
                                    ## NPCC
                                    {
                                      tabPanel("NPCC", value = 1,
                                               
                                               column(width = 4,
                                                      
                                                      br(),
                                                      
                                                      uiOutput('date_range_historic_data_UI'),
                                                      
                                                      div(id = "download_Historic_Data_div",downloadBttn("download_Historic_Data", "Download", 
                                                                                                         size = "sm", color = "default", 
                                                                                                         style = "unite"), align = "left"),
                                                      
                                                      div(actionBttn("historic_data_plot_button",
                                                                     label = "Run",
                                                                     style = "unite",
                                                                     color = "default",
                                                                     size = "sm"), align = "center"),
                                                      
                                                      HTML("<h4><b>Stats</b></h4>"),
                                                      
                                                      withSpinner(uiOutput("historic_data_stats"))
                                                      
                                               ),
                                               
                                               
                                               column(width = 8,
                                                      
                                                      #title = "Historic Weekday Average Load",
                                                      
                                                      withSpinner(plotlyOutput("historic_data_pie_plotly"))
                                                      
                                                      
                                               ),
                                               
                                               column(width = 12, 
                                                      
                                                      hr(),
                                                      
                                                      HTML("<h4><b>Hourly Load Boxplots</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("historic_data_Hourly_boxplot_plotly"))
                                                      
                                               ),
                                               
                                               column(width = 12,
                                                      
                                                      hr(),
                                                      
                                                      HTML("<h4><b>Average Heat Map</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("historic_heatmap_plotly"))
                                                      
                                               ),
                                               
                                               column(width = 12,
                                                      
                                                      hr(),
                                                      
                                                      HTML("<h4><b>Load Duration Curve</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("historic_LDC_plotly"))
                                                      
                                               ),
                                               
                                               column(width = 12,
                                                      
                                                      hr(),
                                                      
                                                      HTML("<h4><b>Load Profile</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("historic_data_line_plotly"))
                                                      
                                               )
                                               
                                      )
                                      
                                    },
                                    
                                    
                                    ## DISCOs
                                    {
                                      
                                      tabPanel("DISCOs",
                                               
                                               column(width = 4, value = 2,
                                                      
                                                      br(),
                                                      
                                                      uiOutput('DISCO_date_range_historic_data_UI'),
                                                      
                                                      div(id = "DISCO_download_Historic_Data_div",
                                                          downloadBttn("DISCO_download_Historic_Data", "Download", 
                                                                       size = "sm", color = "default", 
                                                                       style = "unite"), align = "left"),
                                                      
                                                      div(actionBttn("DISCO_historic_data_plot_button",
                                                                     label = "Run",
                                                                     style = "unite",
                                                                     color = "default",
                                                                     size = "sm"), align = "center"),
                                                      
                                                      hr(),
                                                      
                                                      HTML("<h4><b>Stats</b></h4>"),
                                                      
                                                      withSpinner(dataTableOutput("DISCO_historic_data_stats_table")),
                                                      
                                                      br(),
                                                      
                                                      uiOutput("DISCO_historic_tab_DISCO_selection_UI")
                                                      
                                                      
                                               ),
                                               
                                               column(width = 8,
                                                      
                                                      HTML("<h4><b>Daily Energy Share</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("DISCO_barplot")),
                                                      
                                                      
                                               ),
                                               
                                               
                                               
                                               hr(),
                                               
                                               
                                               
                                               
                                               column(width = 12, br(), uiOutput("Historic_DISCO_tabs"))
                                               
                                      )
                                      
                                    }
                                    
                        )
                        
                    )
                    
                  )
                  
          )
        },
        
        # Model Performance tab
        {
          tabItem(tabName = "model_performance",
                  
                  fluidRow(
                    
                    box(width = 12, 
                        
                        tabsetPanel(type = "tabs", id = "model_performance_tab",
                                    
                                    ## NPCC
                                    {
                                      tabPanel("NPCC", value = 1,
                                               
                                               
                                               column(width = 4,
                                                      
                                                      br(),
                                                      
                                                      div(uiOutput("date_range_model_performance_UI"), 
                                                          align = "left"),
                                                      div(id = "download_model_performance_Data_div", 
                                                          downloadBttn("download_model_performance_Data", 
                                                                       "Download", size = "sm", color = "default", 
                                                                       style = "unite"), align = "left"),
                                                      
                                                      div(actionBttn("model_performance_plot_button",
                                                                     label = "Run",
                                                                     style = "unite",
                                                                     color = "default",
                                                                     size = "sm"), align = "center"),
                                                      
                                                      
                                                      
                                                      
                                                      br(),
                                                      br(),
                                                      
                                                      HTML("<h4><b>Error Metrics</b></h4>"),
                                                      
                                                      withSpinner(uiOutput("model_performance_error_metrics")),
                                                      
                                                      br(),
                                                      br()
                                               ),
                                               
                                               column(width = 8,
                                                      
                                                      br(),
                                                      
                                                      HTML("<h4><b>Actual & Forecast Distribution</b></h4>"), 
                                                      
                                                      withSpinner(plotlyOutput("model_performance_error_distribution_plotly")),
                                                      
                                                      br(),
                                                      br()
                                               ),
                                               
                                               
                                               
                                               column(width = 12, HTML("<h4><b>Actual & Forecast</b></h4>"), 
                                                      
                                                      withSpinner(plotlyOutput("model_performance_forecast_and_actual_plotly"))
                                                      
                                               )
                                               
                                      )
                                      
                                    },
                                    
                                    ## DISCOs
                                    {
                                      tabPanel("DISCOs", value = 2,
                                               
                                               column(width = 4,
                                                      
                                                      br(),
                                                      
                                                      div(uiOutput("DISCOs_date_range_model_performance_UI"),
                                                          align = "left"),
                                                      
                                                      div(id = "DISCO_download_model_performance_Data_div",
                                                          downloadBttn("DISCO_download_model_performance_Data",
                                                                       "Download", size = "sm", color = "default",
                                                                       style = "unite"), align = "left"),
                                                      
                                                      div(actionBttn("DISCOs_model_performance_plot_btn",
                                                                     label = "Run",
                                                                     style = "unite",
                                                                     color = "default",
                                                                     size = "sm"), align = "center"),
                                                      
                                                      br(),
                                                      
                                                      HTML("<h4><b>DISCOs Error Metrics</b></h4>"),
                                                      
                                                      withSpinner(dataTableOutput("DISCO_error_metrics_table"))
                                               ),
                                               
                                               column(width = 8,
                                                      
                                                      HTML("<h4><b>Error Distribution</b></h4>"),
                                                      withSpinner(plotlyOutput("DISCO_model_performance_error_distribution_plot"))
                                                      
                                                      
                                               ),
                                               
                                               column(width = 12,
                                                      
                                                      br(),
                                                      br(),
                                                      
                                                      HTML("<h4><b>Actual & Forecast</b></h4>"),
                                                      
                                                      withSpinner(plotlyOutput("DISCO_model_performance_forecast_and_actual_plotly",
                                                                               height = "800px"))
                                               )
                                               
                                               
                                      )
                                      
                                    }
                                    
                                    
                        )
                    )
                    
                  )
          )
        }
        
      )
    )
  )
  
}




server <- function(input, output, session) {
  
  ## reactive values
  { 
    
    RV <- reactiveValues(
      
      ## User logged in or not
      logged_in_status = NULL,
      
      ## Name of the user
      Username = NULL,
      
      ## index of the user in profiles.csv
      profiles_index = NULL,
      
      ## Not-Access to the particular user
      Exclusion = NULL,
      
      ## Profiles to define access
      profiles = read.csv("profiles.csv"),
      
      Forecast_for_Tomorrow = NULL,
      
      Holidays_2016_2023 = read.csv("Holidays_2016_2023.csv"),
      
      week_forecast = NULL,
      
      model_training = FALSE,
      
      Forecast_status = NULL,
      
      Historic_NPCC_Load_Forecast_Weather = NULL,
      
      new_data_required_date = NULL,
      
      upload_data_tab_new_data = NULL,
      
      historic_data_tab_data = NULL,
      
      model_performance_tab_data = NULL,
      
      loading_modal_message = "Please wait...Do not close tab",
      
      
      ## Cities for which data is used in model for factor
      Cities = data.frame(City = c("Lahore", "Multan", "Faislabad", "Quetta", "Peshawar", 
                                   "Gujranwala", "Hyderabad", "Sukkur", "Islamabad"),
                          Lat = c(31.4831569, 30.1811818, 31.4237883, 30.1800523, 33.9774984, 32.1582524, 25.38371, 27.717682, 33.6163723),
                          Lng = c(74.1943055, 71.3345731, 72.9492131, 66.8786008, 71.4253827, 74.024525, 68.2968651, 68.7986343, 72.805909)),
      
      
      ##### DISCOs ######
      
      DISCO_model_training = FALSE,
      
      PITC_Forecast_for_Tomorrow = NULL,
      
      PITC_Historic_Forecast = NULL,
      
      Historic_DISCO_Load = NULL,
      
      DISCO_week_forecast = NULL,
      
      DISCOs_names = c("PESCO", "TESCO", "IESCO", "GEPCO", "LESCO", 
                       "FESCO", "MEPCO", "SEPCO", "HESCO", "QESCO"),
      
      DISCO_predicion_plot = NULL,
      
      ## Data upload
      All_DISCO_Load_Maintenance = NULL,
      DISCO_Load_Maintenance = NULL,
      
      Editted_data = NULL,
      
      
      DISCO_historic_tab_data = NULL,
      
      DISCO_historic_line_plot = NULL,
      
      DISCO_historic_boxplot_plot = NULL,
      
      
      ### Model Performance ###
      Predicted_historic_DISCO = NULL
      
      
    )
    
  }
  
  
  ## manage download button visibility
  observe({
    
    if(req(RV$Logged_in_status)){
      
      
      req(input$sidebar_tab_selected)
      
      Exclusions <- unlist(strsplit(
        as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
      
      req(input$sidebar_tab_selected)
      
      hide("download_forecast_Data_div")
      hide("download_Historic_Data_div")
      hide("download_model_performance_Data_div")
      
      hide("DISCO_download_forecast_Data_div")
      hide("DISCO_download_model_performance_Data_div")
      
      
      if(!any(Exclusions == "download_buttons")){
        
        
        if(!is.null(RV$Forecast_for_Tomorrow)){
          
          shinyjs::show("download_forecast_Data_div")
          
        }
        
        if(!is.null(RV$PITC_Forecast_for_Tomorrow)){
          
          shinyjs::show("DISCO_download_forecast_Data_div")
          
        }
        
        if(!is.null(RV$historic_data_tab_data)){
          
          shinyjs::show("download_Historic_Data_div")
          
        }
        
        if(!is.null(RV$model_performance_tab_data)){
          
          shinyjs::show("download_model_performance_Data_div")
          
        }
        
        if(!is.null(RV$Predicted_historic_DISCO)){
          
          shinyjs::show("DISCO_download_model_performance_Data_div")
          
        }
        
      }
      
      
      
      
      hide("DISCO_save_new_data_sample_file")
      
      
      DISCOs_names <- req(RV$DISCOs_names)
      
      profile_name <- RV$profiles$Username[req(RV$profiles_index)]
      
      if(any(DISCOs_names == profile_name)){
        
        shinyjs::show("DISCO_save_new_data_sample_file")
        
      }
      
      
      
    }
    
  })
  
  
  
  
  ## Data Downloader
  observe({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Downloading Data ... Please Wait")
    
    RV$Forecast_for_Tomorrow <- tryCatch(download_files_from_google_drive("Forecast_for_Tomorrow.csv"), 
                                         error = function(e){NULL})
    
    RV$week_forecast <- tryCatch(download_files_from_google_drive("week_forecast.csv"), 
                                 error = function(e){NULL})
    
    RV$Historic_NPCC_Load_Forecast_Weather <- tryCatch(download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather.csv"), 
                                                       error = function(e){NULL})
    
    
    RV$Historic_DISCO_Load <- tryCatch(download_files_from_google_drive("Historic_DISCO_Load.csv"), 
                                       error = function(e){NULL})
    
    RV$PITC_Forecast_for_Tomorrow <- tryCatch(download_files_from_google_drive("PITC_Forecast_for_Tomorrow.csv"), 
                                              error = function(e){NULL})
    
    RV$PITC_Historic_Forecast <- tryCatch(download_files_from_google_drive("PITC_Historic_Forecast.csv"), 
                                          error = function(e){NULL})
    
    RV$DISCO_week_forecast <- tryCatch(download_files_from_google_drive("DISCO_week_forecast.csv"), 
                                       error = function(e){NULL})
    
    RV$All_DISCO_Load_Maintenance <- tryCatch(download_files_from_google_drive("DISCO_Load_Maintenance.csv"), 
                                              error = function(e){NULL})
    
    # remove it when done
    remove_modal_gif() 
    
  })
  
  
  
  
  ############### Access / Exclusion ##############
  
  output$menu <- renderMenu({
    
    sidebarMenu( id = "sidebar_tab_selected",
                 
                 if(req(RV$Logged_in_status)){
                   
                   Exclusions <- unlist(strsplit(
                     as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
                   
                   menu = list()
                   count = 1
                   
                   if(!any(Exclusions == "home")){
                     
                     menu[[count]] = menuItem("Home", 
                                              tabName = "home", 
                                              icon = icon("home"))
                     
                     count = count + 1
                   }
                   
                   if(!any(Exclusions == "upload_new_data")){
                     
                     menu[[count]] = menuItem("Upload New Data", 
                                              tabName = "upload_new_data", 
                                              icon = icon("upload"))
                     
                     count = count + 1
                   }
                   
                   if(!any(Exclusions == "historic_data")){
                     
                     menu[[count]] = menuItem("Historic Data Analysis", 
                                              tabName = "historic_data", 
                                              icon = icon("chart-line"))
                     
                     count = count + 1
                   }
                   
                   if(!any(Exclusions == "model_performance")){
                     
                     menu[[count]] = menuItem("Model Performance",
                                              tabName = "model_performance",
                                              icon = icon("tachometer-alt"))
                     
                     count = count + 1
                   }
                   
                   menu
                   
                 }
                 
    )
  })
  
  ## Manage access of NPCC and DISCO tabs
  observe({
    
    
    if(req(RV$Logged_in_status)){
      
      req(input$sidebar_tab_selected)
      
      Exclusions <- unlist(strsplit(
        as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
      
      req(input$sidebar_tab_selected)
      
      
      DISCOs_names <- req(RV$DISCOs_names)
      
      profile_name <- RV$profiles$Username[req(RV$profiles_index)]
      
      if(any(DISCOs_names == profile_name)){
        
        hideTab(inputId = "Home_tab", target = "1")
        hideTab(inputId = "model_performance_tab", target = "1")
        hideTab(inputId = "upload_tab", target = "1")
        hideTab(inputId = "DISCO_historic_tab", target = "1")
        
      }
      
      
      if(!any(Exclusions == "DISCO_forecast")){
        
        hideTab(inputId = "Home_tab", target = "2")
        
        
      }
      
      if(!any(Exclusions == "DISCO_performance")){
        
        hideTab(inputId = "model_performance_tab", target = "2")
        
        
      }
      
      if(!any(Exclusions == "DISCO_upload_tab")){
        
        hideTab(inputId = "upload_tab", target = "2")
        
      }
      
      if(!any(Exclusions == "DISCO_historic")){
        
        hideTab(inputId = "DISCO_historic_tab", target = "2")
        
        
      }
      
      
    }
    
    
    
    
  })
  
  
  
  #################### Login ######################
  
  ## The log-in Dailog box
  {
    
    showModal(modalDialog(
      title = HTML('<img src="logo_resized.png" width="20%" height="20%" align="middle">'),
      h3("Enter details provided by your admin"),
      textInput("username", "Username:", "admin"),
      passwordInput("password", "Password:", "energy"),
      br(),
      actionBttn("login", "Login", style = "fill", color = "default"),
      br(),
      footer = h5("Contact ahmad.nadeem@lums.edu.pk or call 0335-4563928 in case of any problem")
    ))
    
  }
  
  
  ## Login processing
  observe({
    
    ## This Observe requires "Log In" button to be clicked to run
    req(input$login)
    
    ## isolate stops updating if input$username or input$password changes
    isolate({
      Username <- input$username
      Password <- input$password
    })
    
    ## Check which username from profiles.csv matches the input username
    Id.username <- which(RV$profiles$Username == Username)
    
    ## Check which password from profiles.csv matches the input password
    Id.password <- which(RV$profiles$Password == Password)
    
    ## Check if there are any matches for username and password separately
    if (length(Id.username) > 0 & length(Id.password) > 0) {
      
      ## Check if the indices of username and password matches
      if (Id.username == Id.password) {
        
        ## Save the user index in a reactive variable to be used globally
        RV$profiles_index = Id.username
        
        ## Save the username in a reactuve variable to be used globally
        RV$Username = RV$profiles$Username[Id.username]
        
        ## Save the Exclusion in a reactuve variable to be used globally
        RV$Exclusion = RV$profiles$Exclusion[Id.username]
        
        ## Remove the log in dialog box
        removeModal()
        
        log_entry(RV$Username, "Logged In")
        
        ## Approve user, reactive logged_in flag is raised
        RV$Logged_in_status = TRUE
        
      } else {
        
        log_entry(isolate(input$username), paste("Tried to log In with password",
                                                 isolate(input$password)))
        
        # The user enetered the wrong username or password, showing a new dialog box to re-eneter credentials
        showModal(modalDialog(
          title = "Enter details provided by your admin",
          textInput("username", "Username:"),
          passwordInput("password", "Password:"),
          actionBttn("login", "Log In", style = "fill", color = "default"),
          footer = HTML(paste("<font color='red'>Incorrect Username or Password</font><br>
                                    Contact ahmad.nadeem@lums.edu.pk or call 0335-4563928 in case of any problem"))
          
          
        ))
        
      }
      
    } else {
      
      log_entry(isolate(input$username), paste("Tried to log In with password",
                                               isolate(input$password)))
      
      ## The username or password entered does not exist, showing a new dialog box to re-eneter credentials
      showModal(modalDialog(
        title = "Enter details provided by your admin",
        textInput("username", "Username:"),
        passwordInput("password", "Password:"),
        actionBttn("login", "Log In", style = "fill", color = "default"),
        footer = HTML(paste("<font color='red'>Incorrect Username or Password</font><br>
                                    Contact ahmad.nadeem@lums.edu.pk or call 0335-4563928 in case of any problem"))
      ))
      
    }
  }) 
  
  
  #################### Home ########################
  
  #### NPCC Train New Model ####
  observe({
    
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Trainging Fresh Model ... Please Wait")
    
    RV$model_training = FALSE
    
    ## load the previously trained model
    load("NN_model.RData")
    
    ## Only train new model if its more than 30 days since the last training and its past 10 am
    # if(as.numeric(days_since_last_training - Sys.Date()) > 30 &
    #     Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT")))
    
    #####uncomment this everytimeeee
    if(FALSE)
    {
      
      
      ## Call function to get lattest historic data.
      ## This will only get the data stored in Historic_NPCC_Load_Forecast_Weather.csv
      NPCC_NEW <- RV$Historic_NPCC_Load_Forecast_Weather
      
      ##to read Historic_NPCC_Load_Forecast_Weather from file
      # library(readr)
      # NPCC_NEW <- read_csv("Historic_NPCC_Load_Forecast_Weather.csv")
      
      ## Get old data too
      NPCC_OLD <- download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
      
      NPCC <- rbind(NPCC_OLD, NPCC_NEW)[,c("Time", "hourly_load", "Lahore_apparentTemperature")]
      
      ### Lag 48 ### (daily lag)
      NPCC$Lag48 = shift(NPCC$hourly_load, 48)
      
      ### Lag 168 ### (weekly lag)
      NPCC$Lag168 = shift(NPCC$hourly_load, 168)
      
      ### Lag 8760 ### (yearly lag)
      NPCC$Lag8760 = shift(NPCC$hourly_load, 8760)
      
      
      ## Only keep complete cases
      NPCC = NPCC[complete.cases(NPCC),]
      
      ## Filter out data from date from which lahore apparent temperature has no NAs
      NPCC <- NPCC[NPCC$Time >= as.POSIXct("2019-05-01 PKT"), ]
      
      Season = get_season_data(NPCC$Time[1], NPCC$Time[nrow(NPCC)])
      NPCC = merge(NPCC, Season, by = "Time")
      
      ##Working here..adding week day label
      Day_label = get_week_day_data(NPCC$Time[1], NPCC$Time[nrow(NPCC)])
      NPCC = merge (NPCC, Day_label)
      
      # library(tidyverse)
      # #renaming days columns name
      # NPCC  %>% rename(
      #   Sunday = Time.1,
      #   Monday = Time.2, Tuesday = Time.3, Wednesday = Time.4, Thursday = Time.5, Friday = Time.6, Saturday = Time.7)
      # 
      #scaled does not include time column
      scaled = as.data.frame(NPCC[,-1])
      
      cols <- c("hourly_load","Lag48", "Lag168", "Lag8760","Lahore_apparentTemperature")
      
      for ( i in 1:length(cols) ){
        
        max_NPCC = max(NPCC[,cols[i]] )
        min_NPCC = min(NPCC[,cols[i]] )
        
        scaled[, cols[i]] <- scale(NPCC[,cols[i]], 
                                   center = min_NPCC, 
                                   scale = max_NPCC - min_NPCC)               
      }
      
      scaled[, c(1:5)] = lapply(scaled[ ,c(1:5)], as.numeric)
      
      cols <- c("hourly_load","Lag48", "Lag168", "Lag8760","Lahore_apparentTemperature", "Time.1", "Time.2","Time.3","Time.4","Time.5","Time.6","Time.7")
      
      
      
      
      ##split dataset into train test
      # install.packages("ISLR")
      
      # library(ISLR)
      # attach(Smarket)
      # smp_siz = floor(0.75*nrow(scaled))  # creates a value for dividing the data into train and test. In this case the value is defined as 75% of the number of rows in the dataset
      # smp_siz  # shows the value of the sample size
      # 
      # set.seed(123)   # set seed to ensure you always have same random numbers generated
      # train_ind = sample(seq_len(nrow(scaled)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
      # train =scaled[train_ind,] #creates the training dataset with row numbers stored in train_ind
      # test=scaled[-train_ind,]  # creates the test dataset excluding the row numbers mentioned in train_ind
      # 
      # 
      # 
      # # fit neural network
      # NN_winter = neuralnet(hourly_load~., data = train[train$Season == "Winter",-6],
      #                       hidden = c(6, 12, 6),
      #                       linear.output = T, stepmax = 1e7, lifesign = 'full')
      # 
      # # fit neural network
      # NN_summer = neuralnet(hourly_load~., data = train[train$Season == "Summer",-6],
      #                       hidden = c(6, 12, 6),
      #                       linear.output = T, stepmax = 1e7, lifesign = 'full')
      # 
      # # fit neural network
      # NN_autumn = neuralnet(hourly_load~., data = train[train$Season == "Autumn",-6],
      #                       hidden = c(6, 12, 6),
      #                       linear.output = T, stepmax = 1e7, lifesign = 'full')
      # 
      # # fit neural network
      # NN_spring = neuralnet(hourly_load~., data = train[train$Season == "Spring",-6],
      #                       hidden = c(6, 12, 6),
      #                       linear.output = T, stepmax = 1e7, lifesign = 'full')
      
      ## Save the model and supporting files
      # save(NPCC, NN_winter, NN_summer, NN_autumn, NN_spring, file = "NN_modelsplitdata.RData")
      
      
      
      
      # fit neural network
      NN_winter = neuralnet(hourly_load~., data = scaled[scaled$Season == "Winter",-6], 
                            hidden = c(6, 12, 6),
                            linear.output = T, stepmax = 1e7, lifesign = 'full')
      
      # fit neural network
      NN_summer = neuralnet(hourly_load~., data = scaled[scaled$Season == "Summer",-6], 
                            hidden = c(6, 12, 6),
                            linear.output = T, stepmax = 1e7, lifesign = 'full')
      
      # fit neural network
      NN_autumn = neuralnet(hourly_load~., data = scaled[scaled$Season == "Autumn",-6], 
                            hidden = c(6, 12, 6),
                            linear.output = T, stepmax = 1e7, lifesign = 'full')
      
      # fit neural network
      NN_spring = neuralnet(hourly_load~., data = scaled[scaled$Season == "Spring",-6], 
                            hidden = c(6, 12, 6),
                            linear.output = T, stepmax = 1e7, lifesign = 'full')
      
      ## Save the model and supporting files
      save(NPCC, NN_winter, NN_summer, NN_autumn, NN_spring, file = "NN_model.RData")
      
    }
    
    # remove it when done
    remove_modal_gif() 
    
    RV$model_training = TRUE
    
  })
  
  
  #### NPCC Calculating New Forecast ####
  observe({
    
    if(req(input$sidebar_tab_selected) == "home"){
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      req(RV$model_training)
      
      loading_screen("Forecasting Load ... Please Wait")
      
      ## disable download of latest forecast data
      # disable('download_forecast_Data')
      
      if(Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT"))){
        
        
        cat("Calculating New Forecast \n")
        
        ## Call function to get lattest forecast data frame
        Forecast_for_Tomorrow <- RV$Forecast_for_Tomorrow
        
        ## Call function to get lattest historic data. 
        ## This will only get the data stored in Historic_NPCC_Load_Forecast_Weather.csv
        Historic_NPCC_Load_Forecast_Weather <- RV$Historic_NPCC_Load_Forecast_Weather
        
        ## Get old data too
        NPCC_OLD <- download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
        
        
        
        
        ## Make a copy of historic data
        Historic_NPCC_Load_Forecast_Weather_Original = Historic_NPCC_Load_Forecast_Weather
        
        
        Historic_NPCC_Load_Forecast_Weather <- rbind(NPCC_OLD, Historic_NPCC_Load_Forecast_Weather)[,c("Time", "hourly_load")]
        
        
        ## We only need the first two columns: Time and hourly_load
        # Historic_NPCC_Load_Forecast_Weather = Historic_NPCC_Load_Forecast_Weather[,c(1,2)]
        
        
        
        
        if(!is.null(Forecast_for_Tomorrow)){
          
          ## Check if the Forecast_for_Tomorrow.csv has the latest forecast i.e. for the next day
          if(Forecast_for_Tomorrow$Time[1] != as.POSIXct(paste0((Sys.Date()+1)," PKT"))){
            
            need_new_forecast = TRUE
            
          } else {
            
            need_new_forecast = FALSE
            
          }
          
        } else{
          
          need_new_forecast = TRUE
          
        }
        
        
        
        
        if(need_new_forecast){
          
          ## Load the .RData file which has our Neural network
          load("NN_model.RData")
          
          
          ## Calculate the number of rows to add to historic data to create empty data until
          ## tomorrow for which the forecast is required.
          hour_difference = 24* as.vector(as.POSIXct(paste0((Sys.Date()+1)," 23:00:00 PKT"))-
                                            Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)])
          
          ## Create extra rows with empty rows
          Historic_NPCC_Load_Forecast_Weather = rbind(Historic_NPCC_Load_Forecast_Weather,
                                                      data.frame(Time = rep(NA, hour_difference),
                                                                 hourly_load =rep(NA,hour_difference)))
          
          
          
          ## Add time data to time rows
          Historic_NPCC_Load_Forecast_Weather$Time =
            seq.POSIXt(from = Historic_NPCC_Load_Forecast_Weather$Time[1],
                       to = as.POSIXct(paste0((Sys.Date()+1)," 23:00:00 PKT")), by = "hour")
          
          
          
          ## Create 48-hour Lag. This is used as factor to forecast load.
          Historic_NPCC_Load_Forecast_Weather$Lag48 =  
            shift(Historic_NPCC_Load_Forecast_Weather$hourly_load, 48)
          
          ## Create 168-hour Lag. This is used as factor to forecast load.
          Historic_NPCC_Load_Forecast_Weather$Lag168 =  
            shift(Historic_NPCC_Load_Forecast_Weather$hourly_load, 168)
          
          ## Create 8760-hour Lag. This is used as factor to forecast load.
          Historic_NPCC_Load_Forecast_Weather$Lag8760 =  
            shift(Historic_NPCC_Load_Forecast_Weather$hourly_load, 8760)
          
          
          ## Filter data for the last 24 rows, which will be the day for which we need forecast
          Historic_NPCC_Load_Forecast_Weather =
            Historic_NPCC_Load_Forecast_Weather[(nrow(Historic_NPCC_Load_Forecast_Weather)-23):nrow(Historic_NPCC_Load_Forecast_Weather),]
          
          
          cat("Calculating New Forecast !!!")
          
          if(any(!is.na(Historic_NPCC_Load_Forecast_Weather$Lag48)) & any(!is.na(Historic_NPCC_Load_Forecast_Weather$Lag168))& 
             any(!is.na(Historic_NPCC_Load_Forecast_Weather$Lag8760)))
            # if(FALSE)
          {
            
            ## Get Weather Data
            {
              
              Cities <- RV$Cities
              
              # Cities = data.frame(City = c("Lahore", "Multan", "Faislabad", "Quetta", "Peshawar",
              #                              "Gujranwala", "Hyderabad", "Sukkur", "Islamabad"),
              #                     Lat = c(31.4831569, 30.1811818, 31.4237883, 30.1800523, 33.9774984, 32.1582524, 25.38371, 27.717682, 33.6163723),
              #                     Lng = c(74.1943055, 71.3345731, 72.9492131, 66.8786008, 71.4253827, 74.024525, 68.2968651, 68.7986343, 72.805909))
              
              
              Cities <- Cities[ Cities$City == "Lahore" , ]
              
              ## dates for which weather data is required
              dates = unique(substr(as.character(Historic_NPCC_Load_Forecast_Weather$Time), 1, 10))
              
              ## Initialize dataframe that will contain weather data
              Cities_apparent_temperature = data.frame(Time = Historic_NPCC_Load_Forecast_Weather$Time)
              
              ## loop for each city
              for(i in 1:nrow(Cities)){
                
                temp = NULL
                
                ## loop for each date
                for(j in 1:length(dates)){
                  
                  cat("\014")
                  cat("Getting data for", Cities$City[i], dates[j])
                  
                  ## weather data for each city for each day
                  weather_data = get_weather_data(dates[j], Cities$Lat[i], Cities$Lng[i])
                  temp = rbind(temp,  weather_data)
                  
                }
                
                ## Change name of column
                colnames(temp)[2:ncol(temp)] = paste0(Cities$City[i],"_", colnames(temp)[2:ncol(temp)])
                
                ## merge apparent temperature data for each city
                Cities_apparent_temperature = merge(Cities_apparent_temperature, temp, by = "Time")
                
              }
              
              Historic_NPCC_Load_Forecast_Weather = merge(Historic_NPCC_Load_Forecast_Weather,
                                                          Cities_apparent_temperature[, c("Time", "Lahore_apparentTemperature")], all = TRUE)
              
            }
            
            
            Season = get_season_data(Historic_NPCC_Load_Forecast_Weather$Time[1], 
                                     Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)])
            Historic_NPCC_Load_Forecast_Weather = merge(Historic_NPCC_Load_Forecast_Weather, Season, by = "Time")
            
            
            
            ##Working here..adding week day label
            Day_label = get_week_day_data(Historic_NPCC_Load_Forecast_Weather$Time[1], 
                                          Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)])
            Historic_NPCC_Load_Forecast_Weather = merge (Historic_NPCC_Load_Forecast_Weather, Day_label, by = "Time")
            
            # library(tidyverse)
            # #renaming days columns name
            # Historic_NPCC_Load_Forecast_Weather  %>% rename(
            #   Sunday = Time.1,
            #   Monday = Time.2, Tuesday = Time.3, Wednesday = Time.4, Thursday = Time.5, Friday = Time.6, Saturday = Time.7)
            # 
            # 
            
            scaled_NPCC_Prediction = as.data.frame(Historic_NPCC_Load_Forecast_Weather[,-1])
            
            cols <- c("hourly_load","Lag48", "Lag168","Lag8760", "Lahore_apparentTemperature")
            
            for ( i in 1:length(cols) ){
              
              max_NPCC = max(NPCC[,cols[i]] )
              min_NPCC = min(NPCC[,cols[i]] )
              
              scaled_NPCC_Prediction[, cols[i]] <- scale(scaled_NPCC_Prediction[,cols[i]], 
                                                         center = min_NPCC, 
                                                         scale = max_NPCC - min_NPCC)               
            }
            
            scaled_NPCC_Prediction[, c(1:5)] = lapply(scaled_NPCC_Prediction[ ,c(1:5)], as.numeric)
            # test[, c(1:5)] = lapply(test[ ,c(1:5)], as.numeric)
            # 
            # 
            # 
            # 
            # if(scaled_NPCC_Prediction$Season[1] == "Winter"){
            #   
            #   ## Calculate forecast
            #   predict_NPCC_Prediction = neuralnet::compute(NN_winter, scaled_NPCC_Prediction[,-1])
            #   
            # } else if(scaled_NPCC_Prediction$Season[1] == "Summer"){
            #   
            #   ## Calculate forecast
            #   predict_NPCC_Prediction = neuralnet::compute(NN_summer, scaled_NPCC_Prediction[,-1])
            #   
            # } else if(scaled_NPCC_Prediction$Season[1] == "Autumn"){
            #   
            #   ## Calculate forecast
            #   predict_NPCC_Prediction = neuralnet::compute(NN_autumn, scaled_NPCC_Prediction[,-1])
            #   
            # } else if(scaled_NPCC_Prediction$Season[1] == "Spring"){
            #   
            #   ## Calculate forecast
            #   predict_NPCC_Prediction = neuralnet::compute(NN_spring, scaled_NPCC_Prediction[,-1])
            #   
            # }
            
            
            
            if(scaled_NPCC_Prediction$Season[1] == "Winter"){
              
              ## Calculate forecast
              predict_NPCC_Prediction = neuralnet::compute(NN_winter, scaled_NPCC_Prediction[,-1])
              
            } else if(scaled_NPCC_Prediction$Season[1] == "Summer"){
              
              ## Calculate forecast
              predict_NPCC_Prediction = neuralnet::compute(NN_summer, scaled_NPCC_Prediction[,-1])
              
            } else if(scaled_NPCC_Prediction$Season[1] == "Autumn"){
              
              ## Calculate forecast
              predict_NPCC_Prediction = neuralnet::compute(NN_autumn, scaled_NPCC_Prediction[,-1])
              
            } else if(scaled_NPCC_Prediction$Season[1] == "Spring"){
              
              ## Calculate forecast
              predict_NPCC_Prediction = neuralnet::compute(NN_spring, scaled_NPCC_Prediction[,-1])
              
            }
            
            
            ## Rescale forecast
            predict_NPCC_Prediction = (predict_NPCC_Prediction$net.result *
                                         (max(NPCC$hourly_load, na.rm = T) -
                                            min(NPCC$hourly_load, na.rm = T))) + min(NPCC$hourly_load, na.rm = T)
            
            Historic_NPCC_Load_Forecast_Weather$Predicted = as.vector(predict_NPCC_Prediction)
            
            ##Get weather data of all cities
            {
              
              Cities <- RV$Cities
              
              # Cities = data.frame(City = c("Lahore", "Multan", "Faislabad", "Quetta", "Peshawar",
              #                              "Gujranwala", "Hyderabad", "Sukkur", "Islamabad"),
              #                     Lat = c(31.4831569, 30.1811818, 31.4237883, 30.1800523, 33.9774984, 32.1582524, 25.38371, 27.717682, 33.6163723),
              #                     Lng = c(74.1943055, 71.3345731, 72.9492131, 66.8786008, 71.4253827, 74.024525, 68.2968651, 68.7986343, 72.805909))
              
              
              ## dates for which weather data is required
              dates = unique(substr(as.character(Historic_NPCC_Load_Forecast_Weather$Time), 1, 10))
              
              ## Initialize dataframe that will contain weather data
              Cities_apparent_temperature = data.frame(Time = Historic_NPCC_Load_Forecast_Weather$Time)
              
              ## loop for each city
              for(i in 1:nrow(Cities)){
                
                temp = NULL
                
                ## loop for each date
                for(j in 1:length(dates)){
                  
                  cat("\014")
                  cat("Getting data for", Cities$City[i], dates[j])
                  
                  ## weather data for each city for each day
                  weather_data = get_weather_data(dates[j], Cities$Lat[i], Cities$Lng[i])
                  temp = rbind(temp,  weather_data)
                  
                }
                
                ## Change name of column
                colnames(temp)[2:ncol(temp)] = paste0(Cities$City[i],"_", colnames(temp)[2:ncol(temp)])
                
                ## merge apparent temperature data for each city
                Cities_apparent_temperature = merge(Cities_apparent_temperature, temp, by = "Time")
                
              }
              
              Historic_NPCC_Load_Forecast_Weather = merge(Historic_NPCC_Load_Forecast_Weather,
                                                          Cities_apparent_temperature)
              
            }
            
            # accuracy(Historic_NPCC_Load_Forecast_Weather$Predicted, Historic_NPCC_Load_Forecast_Weather$hourly_load)
            ## Save and filter forecast data
            New_Forecast = Historic_NPCC_Load_Forecast_Weather[,colnames(Historic_NPCC_Load_Forecast_Weather_Original)]
            
            
            ## Save old forecast in historic data
            if(any(Historic_NPCC_Load_Forecast_Weather_Original$Time %in% 
                   Forecast_for_Tomorrow$Time)){
              
              Historic_NPCC_Load_Forecast_Weather_Original[which(Historic_NPCC_Load_Forecast_Weather_Original$Time %in% 
                                                                   Forecast_for_Tomorrow$Time),c(2,3)] = Forecast_for_Tomorrow[,c(2,3)]
              
            } else {
              
              ## Save the previous forecast into historic data
              Historic_NPCC_Load_Forecast_Weather_Original =
                merge(Historic_NPCC_Load_Forecast_Weather_Original, Forecast_for_Tomorrow[,c(1,2,3)], all = TRUE)
              
            }
            
            RV$Forecast_for_Tomorrow = New_Forecast
            
            RV$Historic_NPCC_Load_Forecast_Weather = Historic_NPCC_Load_Forecast_Weather_Original
            
            
            # write.csv(New_Forecast, "Forecast_for_Tomorrow.csv" , row.names = FALSE)
            upload_files_to_google_drive("Forecast_for_Tomorrow.csv", New_Forecast)
            
            upload_files_to_google_drive("Historic_NPCC_Load_Forecast_Weather.csv", Historic_NPCC_Load_Forecast_Weather_Original)
            
            
            
            
            
            RV$Forecast_status = paste("<p style='font-size:20px;'>The Forecast for", 
                                       format((Forecast_for_Tomorrow$Time[1]), "%A %B %d, %Y"), "is Available !</p>")
            
            
            
          } else {
            ## If 48-hour old data is not available
            
            RV$Forecast_status = paste("<p style='font-size:20px;'>The Forecast for", format((Sys.Date()+1), "%A %B %d, %Y"), 
                                       "is Not Available !</p>",
                                       "<br> <p style='font-size:20px;color:red;'> Make sure Data for", 
                                       format((Sys.Date()-7), "%A %B %d, %Y"),
                                       # "&", format((Sys.Date()-1), "%A %B %d, %Y"),
                                       "is uploaded Atleast</p>")
            
          }
          
        } else {
          
          ## The forecast for tomorrow is already calculated
          RV$Forecast_status = paste("<p style='font-size:20px;'>The Forecast for", 
                                     format((Forecast_for_Tomorrow$Time[1]), "%A %B %d, %Y"), "is Available !</p>")
          
        }
        
      } else {
        
        ## forecast will be available after 10 am
        RV$Forecast_status = paste("<p style='font-size:20px;'>The Forecast for", 
                                   format((Sys.Date()+1), "%A %B %d, %Y"), "will be available after 10 am</p>")
        
      }
      
      
      ## Enable download of latest forecast data
      # enable('download_forecast_Data')
      
      # remove it when done
      remove_modal_gif()
      
    }
    
  })
  
  
  #### NPCC Calculating Forecast for next 6 days ####
  observe({
    
    if(req(input$sidebar_tab_selected) == "home"){
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      req(RV$model_training)
      
      loading_screen("Forecasting 6 Day Load ... Please Wait")
      
      ## disable download of latest forecast data
      # disable('download_forecast_Data')
      
      if(Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT"))){
        
        
        ## Call function to get latest historic data. 
        ## This will only get the data stored in Historic_NPCC_Load_Forecast_Weather.csv
        Historic_NPCC_Load_Forecast_Weather <- req(RV$Historic_NPCC_Load_Forecast_Weather)
        
        # historic = Historic_NPCC_Load_Forecast_Weather[!is.na(Historic_NPCC_Load_Forecast_Weather$hourly_load),]
        
        
        ## Get old data too
        NPCC_OLD <- download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
        
        week_forecast <- RV$week_forecast
        
        
        
        
        ## Make a copy of historic data
        Historic_NPCC_Load_Forecast_Weather_Original = Historic_NPCC_Load_Forecast_Weather
        
        
        historic <- rbind(NPCC_OLD, Historic_NPCC_Load_Forecast_Weather)[,c("Time", "hourly_load")]
        
        
        
        
        
        if(!is.null(week_forecast)){
          
          ## Check if the week_forecast.csv has the latest forecast i.e. for the next six days
          if(week_forecast$Time[1] != as.POSIXct(paste(Sys.Date()+2, "PKT"))){
            
            week_forecast = NULL
            
          }
          
        }
        
        
        if(is.null(week_forecast)){
          
          # if(FALSE){
          
          # i = 2
          
          for(i in 2:7){
            
            Date_to_forecast = Sys.Date()+i
            
            
            forecast_df = data.frame(Time = seq.POSIXt(from = as.POSIXct(paste0(Date_to_forecast, " PKT")),
                                                       to = as.POSIXct(paste0(Date_to_forecast, "23:00:00 PKT")), by = "hour"))
            
            forecast_df$hourly_load = NA
            
            
            forecast_df$Lag48 = historic$hourly_load[(nrow(historic)-47):(nrow(historic)- 24)]
            forecast_df$Lag168 = historic$hourly_load[(nrow(historic)-167):(nrow(historic)-144)]
            forecast_df$Lag8760 = historic$hourly_load[(nrow(historic)-8759):(nrow(historic)-8736)]
            
            if(any(!is.na(forecast_df$Lag48)) & any(!is.na(forecast_df$Lag168))& 
               any(!is.na(forecast_df$Lag8760)))
              # if(FALSE)
            {
              
              
              ### Get Weather Data ###
              {
                
                Cities <- RV$Cities
                
                # Cities = data.frame(City = c("Lahore", "Multan", "Faislabad", "Quetta", "Peshawar",
                #                              "Gujranwala", "Hyderabad", "Sukkur", "Islamabad"),
                #                     Lat = c(31.4831569, 30.1811818, 31.4237883, 30.1800523, 33.9774984, 32.1582524, 25.38371, 27.717682, 33.6163723),
                #                     Lng = c(74.1943055, 71.3345731, 72.9492131, 66.8786008, 71.4253827, 74.024525, 68.2968651, 68.7986343, 72.805909))
                
                Cities <- Cities[ Cities$City == "Lahore" , ]
                
                ## dates for which weather data is required
                dates = unique(substr(as.character(forecast_df$Time), 1, 10))
                
                ## Initialize dataframe that will contain weather data
                Cities_apparent_temperature = data.frame(Time = forecast_df$Time)
                
                ## loop for each city
                for(i in 1:nrow(Cities)){
                  
                  temp = NULL
                  
                  ## loop for each date
                  for(j in 1:length(dates)){
                    
                    cat("\014")
                    cat("Getting data for", Cities$City[i], dates[j])
                    
                    ## weather data for each city for each day
                    weather_data = get_weather_data(dates[j], Cities$Lat[i], Cities$Lng[i])
                    temp = rbind(temp,  weather_data)
                    
                  }
                  
                  ## Change name of column
                  colnames(temp)[2:ncol(temp)] = paste0(Cities$City[i],"_", colnames(temp)[2:ncol(temp)])
                  
                  ## merge apparent temperature data for each city
                  Cities_apparent_temperature = merge(Cities_apparent_temperature, temp, by = "Time")
                  
                }
                
                forecast_df = merge(forecast_df, 
                                    Cities_apparent_temperature[, c("Time", "Lahore_apparentTemperature")], all = TRUE)
                
              }
              
              
              ## Load the .RData file which has our Neural network
              load("NN_model.RData")
              
              Season = get_season_data(forecast_df$Time[1], 
                                       forecast_df$Time[nrow(forecast_df)])
              
              forecast_df = merge(forecast_df, Season, by = "Time")
              
              
              
              ##Working here..adding week day label
              Day_label <- get_week_day_data(forecast_df$Time[1], 
                                             forecast_df$Time[nrow(forecast_df)])
              forecast_df = merge (forecast_df, Day_label, by = "Time")
              
              # library(tidyverse)
              # #renaming days columns name
              # forecast_df  %>% rename(
              #   Sunday = Time.1,
              #   Monday = Time.2, Tuesday = Time.3, Wednesday = Time.4, Thursday = Time.5, Friday = Time.6, Saturday = Time.7)
              # 
              
              
              scaled_NPCC_Prediction = as.data.frame(forecast_df[,-1])
              
              cols <- c("hourly_load","Lag48", "Lag168","Lag8760", "Lahore_apparentTemperature")
              
              for ( i in 1:length(cols) ){
                # i=2
                max_NPCC = max(NPCC[,cols[i]] )
                min_NPCC = min(NPCC[,cols[i]] )
                
                scaled_NPCC_Prediction[, cols[i]] <- scale(scaled_NPCC_Prediction[,cols[i]], 
                                                           center = min_NPCC, 
                                                           scale = max_NPCC - min_NPCC)               
              }
              
              scaled_NPCC_Prediction[, c(1:5)] = lapply(scaled_NPCC_Prediction[ ,c(1:5)], as.numeric)
              
              
              if(scaled_NPCC_Prediction$Season[1] == "Winter"){
                
                ## Calculate forecast
                predict_NPCC_Prediction = neuralnet::compute(NN_winter, scaled_NPCC_Prediction[,-1])
                
              } else if(scaled_NPCC_Prediction$Season[1] == "Summer"){
                
                ## Calculate forecast
                predict_NPCC_Prediction = neuralnet::compute(NN_summer, scaled_NPCC_Prediction[,-1])
                
              } else if(scaled_NPCC_Prediction$Season[1] == "Autumn"){
                
                ## Calculate forecast
                predict_NPCC_Prediction = neuralnet::compute(NN_autumn, scaled_NPCC_Prediction[,-1])
                
              } else if(scaled_NPCC_Prediction$Season[1] == "Spring"){
                
                ## Calculate forecast
                predict_NPCC_Prediction = neuralnet::compute(NN_spring, scaled_NPCC_Prediction[,-1])
                
              }
              
              
              ## Rescale forecast
              predict_NPCC_Prediction = (predict_NPCC_Prediction$net.result *
                                           (max(NPCC$hourly_load, na.rm = T) -
                                              min(NPCC$hourly_load, na.rm = T))) + min(NPCC$hourly_load, na.rm = T)
              
              forecast_df$Predicted = as.vector(predict_NPCC_Prediction)
              
              
              ##Get weather data of all cities
              {
                
                Cities <- RV$Cities
                
                # Cities = data.frame(City = c("Lahore", "Multan", "Faislabad", "Quetta", "Peshawar",
                #                              "Gujranwala", "Hyderabad", "Sukkur", "Islamabad"),
                #                     Lat = c(31.4831569, 30.1811818, 31.4237883, 30.1800523, 33.9774984, 32.1582524, 25.38371, 27.717682, 33.6163723),
                #                     Lng = c(74.1943055, 71.3345731, 72.9492131, 66.8786008, 71.4253827, 74.024525, 68.2968651, 68.7986343, 72.805909))
                
                ## dates for which weather data is required
                dates = unique(substr(as.character(forecast_df$Time), 1, 10))
                
                ## Initialize dataframe that will contain weather data
                Cities_apparent_temperature = data.frame(Time = forecast_df$Time)
                
                ## loop for each city
                for(i in 1:nrow(Cities)){
                  
                  temp = NULL
                  
                  ## loop for each date
                  for(j in 1:length(dates)){
                    
                    cat("\014")
                    cat("Getting data for", Cities$City[i], dates[j])
                    
                    ## weather data for each city for each day
                    weather_data = get_weather_data(dates[j], Cities$Lat[i], Cities$Lng[i])
                    temp = rbind(temp,  weather_data)
                    
                  }
                  
                  ## Change name of column
                  colnames(temp)[2:ncol(temp)] = paste0(Cities$City[i],"_", colnames(temp)[2:ncol(temp)])
                  
                  ## merge apparent temperature data for each city
                  Cities_apparent_temperature = merge(Cities_apparent_temperature, temp, by = "Time")
                  
                }
                
                forecast_df = merge(forecast_df, Cities_apparent_temperature)
                
              }
              
              
              
              
              ## Save and filter forecast data
              New_Forecast = forecast_df[,colnames(Historic_NPCC_Load_Forecast_Weather_Original)]
              
              # ## Save and filter forecast data
              # New_Forecast = Historic_NPCC_Load_Forecast_Weather[,colnames(Historic_NPCC_Load_Forecast_Weather_Original)]
              # 
              
              week_forecast = rbind(week_forecast, New_Forecast)
              
            }
            
            RV$week_forecast = week_forecast
            
            
            
            
            upload_files_to_google_drive("week_forecast.csv", week_forecast)
            
            
            
            
          }
          
        }
        
      }
    }
    
    ## Enable download of latest forecast data
    # enable('download_forecast_Data')
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  
  #### NPCC Announce that new forecast is available or not ####
  output$forecast_info = renderUI({ HTML(RV$Forecast_status) })
  
  
  ##### NPCC Data Start - End Date #####
  output$date_range_home_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("date_range_home", "Select Date Range", 
                   start = Sys.Date()+1,
                   end = Sys.Date()+1,
                   min = Sys.Date()+1,
                   max = Sys.Date()+7)
    
  })
  
  
  #### NPCC Forecast Plot ####
  output$forecast_plot <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    Forecast_for_Tomorrow <- req(RV$Forecast_for_Tomorrow)
    week_forecast <- req(RV$week_forecast)
    # write.csv(Forecast_for_Tomorrow[,-2],"Forecast_for_Tomorrow.csv", row.names = FALSE)
    
    ## Get lattest forecast data frame
    Forecast_for_Tomorrow <- rbind(Forecast_for_Tomorrow, week_forecast)
    
    Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(req(input$date_range_home))," 00:00:00 PKT")) & 
                                                     Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(req(input$date_range_home))," 23:00:00 PKT")),]
    
    
    if(nrow(Forecast_for_Tomorrow) > 0){
      
      
      Days <- unique(substr(as.character(Forecast_for_Tomorrow$Time), 1, 10))
      
      ## Check if forecast dataframe has any rows or not, Otherwise 
      ## error occurs because R trys to plot empty data frame anyway
      if (length(nrow(Forecast_for_Tomorrow)) != 0) {
        
        
        ## Line plot of Predicted load
        p1 = plot_ly(data = Forecast_for_Tomorrow, x=~Time)%>%
          add_trace(y=~Predicted, name = "Forecast[MW]", type = "scatter", 
                    mode = "lines",color = I('red'), fill = "tozeroy",
                    hoverinfo = "text",  
                    hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                       "<br> Predicted Load :", round(Predicted,2), "MW"))%>% 
          layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
                 yaxis = list( title = "Load [MW]"))
        
        p1
        
      }
      
    }
    
    
  })
  
  
  #### Cities Weather ####
  output$cities_weather_plot <- renderPlotly({
    
    Cities <- req(RV$Cities)
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Log        
    cat("Plotting Cities Weather \n")
    
    ## Get lattest forecast data frame
    Forecast_for_Tomorrow <- rbind(req(RV$Forecast_for_Tomorrow), req(RV$week_forecast))
    # Forecast_for_Tomorrow <- rbind(Forecast_for_Tomorrow, week_forecast)
    
    Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(req(input$date_range_home))," 00:00:00 PKT")) & 
                                                     Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(req(input$date_range_home))," 23:00:00 PKT")),]
    
    
    if(nrow(Forecast_for_Tomorrow) > 0){
      
      
      # Days <- unique(substr(as.character(Forecast_for_Tomorrow$Time), 1, 10))
      
      ## Check if forecast dataframe has any rows or not, Otherwise 
      ## error occurs because R trys to plot empty data frame anyway
      if (length(nrow(Forecast_for_Tomorrow)) != 0) {
        
        
        colors = brewer.pal(n = 10, name = 'Set3')
        
        fig = list()
        
        # i = 1
        
        for(i in 1:nrow(Cities)){
          
          # weather_data = lapply(Days, get_weather_data, latitude = Cities$Lat[i], longitude = Cities$Lng[i] ) %>% bind_rows()
          
          ## Bar plot of temperature and icon status
          fig[[i]] = plot_ly(data = Forecast_for_Tomorrow, x=~Time)%>%
            add_trace(y=Forecast_for_Tomorrow[[paste0(Cities$City[i],"_apparentTemperature")]], name = paste0( Cities$City[i], " Apparent Temperature[\u00B0C]" ), type = "bar", 
                      marker = list(color = colors[i],
                                    line = list(color = '#000',width = 1.5)),
                      hoverinfo = "text", 
                      hovertext =~ paste(paste0(Cities$City[i]," <br>"),
                                         "Time :", format(Time, format = "%d-%b-%Y %I %p"), "<br>",
                                         Forecast_for_Tomorrow[[paste0(Cities$City[i],"_icon")]],
                                         "<br> Apparent Temperature :", Forecast_for_Tomorrow[[paste0(Cities$City[i],"_apparentTemperature")]], "\u00B0C"))%>% 
            layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
                   yaxis = list( title = "Apparent Temperature \u00B0C"))
          
          
        }
        
        plotly::subplot(fig ,nrows = nrow(Cities), shareX = TRUE)
        
      }
      
      
      
    }
    
    
  })
  
  
  #### NPCC Forecast Data Table ####
  output$forecast_data_table_UI = renderDataTable({
    
    
    ## Get latest forecast data frame
    Forecast_for_Tomorrow <- rbind(req(RV$Forecast_for_Tomorrow), req(RV$week_forecast))
    
    Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(req(input$date_range_home))," 00:00:00 PKT")) & 
                                                     Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(req(input$date_range_home))," 23:00:00 PKT")),]
    
    
    if(nrow(Forecast_for_Tomorrow) > 0){
      
      
      Forecast_for_Tomorrow = merge(get_season_data(Forecast_for_Tomorrow$Time[1], 
                                                    Forecast_for_Tomorrow$Time[nrow(Forecast_for_Tomorrow)]),
                                    Forecast_for_Tomorrow,
                                    by = "Time")
      
      
      
      ## Call function to get lattest historic data. 
      ## This will only get the data stored in Historic_NPCC_Load_Forecast_Weather.csv
      Historic_NPCC_Load_Forecast_Weather <- rbind(RV$Historic_NPCC_Load_Forecast_Weather[,c(1,2)], 
                                                   Forecast_for_Tomorrow[,c(1,3)])
      
      
      ## Create 48-hour Lag. This is used as factor to forecast load.
      Historic_NPCC_Load_Forecast_Weather$Lag48 =  shift(Historic_NPCC_Load_Forecast_Weather$hourly_load, 48)
      
      ## Create 168-hour Lag. This is used as factor to forecast load.
      Historic_NPCC_Load_Forecast_Weather$Lag168 =  shift(Historic_NPCC_Load_Forecast_Weather$hourly_load, 168)
      
      
      
      Forecast_for_Tomorrow <- cbind(Historic_NPCC_Load_Forecast_Weather[Historic_NPCC_Load_Forecast_Weather$Time >= Forecast_for_Tomorrow$Time[1] &
                                                                           Historic_NPCC_Load_Forecast_Weather$Time <= Forecast_for_Tomorrow$Time[nrow(Forecast_for_Tomorrow)], c(1,3,4)],
                                     Forecast_for_Tomorrow[,-c(1,3)] )
      
      
      
      
      
      Forecast_for_Tomorrow <- round_df(Forecast_for_Tomorrow[, c(1, 5, 2:4, 6:ncol(Forecast_for_Tomorrow))], 2)
      
      Holidays_2016_2023 = req(RV$Holidays_2016_2023)
      
      
      if(any(unique(as.Date(Forecast_for_Tomorrow$Time, tz = "Asia/Karachi")) == Holidays_2016_2023$Date)){
        
        Forecast_for_Tomorrow$Special_Events = Holidays_2016_2023$Holiday[which(unique(as.Date(Forecast_for_Tomorrow$Time, 
                                                                                               tz = "Asia/Karachi") == Holidays_2016_2023$Date))]
        
      } else {
        
        
        Forecast_for_Tomorrow$Special_Events = "No Special Event"
        
      }
      
      Forecast_for_Tomorrow = Forecast_for_Tomorrow[, c(1:5, 105, 6:ncol(Forecast_for_Tomorrow))]
      
      Forecast_for_Tomorrow$Time = format(Forecast_for_Tomorrow$Time + 3600, format = "%c")
      
      Forecast_for_Tomorrow
      
      
    }
    
    
  }, 
  options = list(scrollX = TRUE, stateSave = TRUE), rownames= FALSE)
  
  
  #### NPCC Forecast Download Handler ####
  output$download_forecast_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("NPCC-Forecast-",
                                 as.character(min(input$date_range_home)),"-", as.character(max(input$date_range_home)), 
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      ## Get lattest forecast data frame
      Forecast_for_Tomorrow <- rbind(req(RV$Forecast_for_Tomorrow)[,c(1,3)], req(RV$week_forecast)[,c(1,3)])
      
      Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(input$date_range_home)," 00:00:00 PKT")) & 
                                                       Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(input$date_range_home)," 23:00:00 PKT")),]
      
      
      Days = unique(substr(as.character(Forecast_for_Tomorrow$Time), 1, 10))
      
      Forecast_for_Tomorrow$Days = rep(Days, each = 24)
      
      Forecast_for_Tomorrow$Hour = 1:24
      
      
      Formatted_Forecast_for_Tomorrow = data.frame( matrix(nrow = (length(Days)), ncol = 25 ) )
      
      Formatted_Forecast_for_Tomorrow$X1 = Days
      
      
      # hour_index = 1
      # day_index = 1
      
      for(hour_index in 1:24){
        
        for (day_index in 1:length(Days)) {
          
          Formatted_Forecast_for_Tomorrow[Formatted_Forecast_for_Tomorrow$X1 == Days[day_index],(hour_index+1)] = as.numeric(Forecast_for_Tomorrow[Forecast_for_Tomorrow$Days == Days[day_index] & 
                                                                                                                                                     Forecast_for_Tomorrow$Hour == hour_index, 2])
        }
      }
      
      colnames(Formatted_Forecast_for_Tomorrow) = c("Day / Hour", paste0("0",1:9,"00"), paste0(10:24,"00"))
      
      
      log_entry(req(RV$Username), "Downloaded Forecast")
      
      
      write.csv(Formatted_Forecast_for_Tomorrow, file, row.names = FALSE)
      
      
    })
  
  
  
  
  
  
  
  ###################### DISCOs #############################
  
  #### DISCOs Train New Model ####
  observe({
    
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Trainging Fresh Model ... Please Wait")
    
    RV$DISCO_model_training = FALSE
    
    ## load the previously trained model
    # load("DISCO_NN_model.RData")
    
    ## Only train new model if its more than 30 days since the last training and its past 10 am
    # if(as.numeric(days_since_last_training - Sys.Date()) > 30 &
    #     Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT")))
    
    
    if(FALSE)
    {
      
      
      Historic_NPCC_Load_Forecast_Weather = req(RV$Historic_NPCC_Load_Forecast_Weather)
      
      Historic_NPCC_Load_Forecast_Weather_OLD = 
        download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
      
      ## Convert Time to posixct
      Historic_NPCC_Load_Forecast_Weather_OLD$Time = as.POSIXct(Historic_NPCC_Load_Forecast_Weather_OLD$Time)
      
      ## Row bind both Old historic data and historic data
      Historic_NPCC_Load_Forecast_Weather <- rbind(Historic_NPCC_Load_Forecast_Weather_OLD, 
                                                   Historic_NPCC_Load_Forecast_Weather)
      
      
      ## This will only get the data stored in Historic_DISCO_Load.csv
      DISCO_NEW <- RV$Historic_DISCO_Load
      
      ## Get old data too
      DISCO_OLD <- download_files_from_google_drive("Historic_DISCO_Load_OLD.csv")
      
      
      
      DISCO <- rbind(DISCO_OLD, DISCO_NEW)
      
      # DISCO_temp = DISCO
      # DISCO = DISCO_temp
      
      
      DISCO = DISCO[, c(1, grep("Drawn", colnames(DISCO)))]
      
      temp =  data.frame(sapply(DISCO[,-1], function(x){shift(x, 168)}))
      
      colnames(temp) = paste0(colnames(temp),"_Lag168")
      
      DISCO = cbind(DISCO, temp)
      
      
      # DISCO = merge(DISCO, 
      #              Historic_NPCC_Load_Forecast_Weather[ ,c(1,grep("apparentTemperature", 
      #                                                             colnames(Historic_NPCC_Load_Forecast_Weather)))])
      # 
      # colnames(DISCO)[colnames(DISCO) == "Lahore_apparentTemperature"] = "LESCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Multan_apparentTemperature"] = "MEPCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Faislabad_apparentTemperature"] = "FESCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Quetta_apparentTemperature"] = "QESCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Peshawar_apparentTemperature"] = "PESCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Gujranwala_apparentTemperature"] = "GEPCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Hyderabad_apparentTemperature"] = "HESCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Sukkur_apparentTemperature"] = "SEPCO_apparentTemperature"
      # colnames(DISCO)[colnames(DISCO) == "Islamabad_apparentTemperature"] = "IESCO_apparentTemperature"
      # DISCO$TESCO_apparentTemperature = DISCO$PESCO_apparentTemperature
      
      
      ## Only keep complete cases
      DISCO = DISCO[complete.cases(DISCO),]
      
      
      
      Season = get_season_data(DISCO$Time[1], DISCO$Time[nrow(DISCO)])
      DISCO = merge(DISCO, Season, by = "Time")
      
      
      
      scaled = as.data.frame(DISCO[,-1])
      
      cols <- colnames(scaled[, -which(colnames(scaled) %in% "Season")])
      
      for ( i in 1:length(cols) ){
        
        max_DISCO = max(DISCO[,cols[i]] )
        min_DISCO = min(DISCO[,cols[i]] )
        
        scaled[, cols[i]] <- scale(DISCO[,cols[i]],
                                   center = min_DISCO,
                                   scale = max_DISCO - min_DISCO)
      }
      
      scaled[, c(1:(ncol(scaled)-1))] = lapply(scaled[ ,c(1:(ncol(scaled)-1))], as.numeric)
      
      
      DISCOs_names = c("LESCO", "MEPCO", "FESCO", "QESCO", 
                       "PESCO", "GEPCO", "HESCO", "SEPCO", 
                       "IESCO", "TESCO")
      
      NN_winter = list()
      NN_summer = list()
      NN_autumn = list()
      NN_spring = list()
      
      hidden_nn = c(3, 6, 3)
      
      # i = 1
      for(i in 1:10){
        
        temp = scaled[,c(grep(DISCOs_names[i], colnames(scaled)), ncol(scaled))]
        colnames(temp) = c("Drawn", "Lag168", "Season")
        
        cat("\014")
        cat("Fitting neural network for:", DISCOs_names[i])
        
        # fit neural network
        NN_winter[[i]] = neuralnet(Drawn~., data = temp[temp$Season == "Winter",-3], hidden = hidden_nn,
                                   linear.output = T, stepmax = 1e7, lifesign = 'full')
        
        # fit neural network
        NN_summer[[i]] = neuralnet(Drawn~., data = temp[temp$Season == "Summer",-3], hidden = hidden_nn,
                                   linear.output = T, stepmax = 1e7, lifesign = 'full')
        
        # fit neural network
        NN_autumn[[i]] = neuralnet(Drawn~., data = temp[temp$Season == "Autumn",-3], hidden = hidden_nn,
                                   linear.output = T, stepmax = 1e7, lifesign = 'full')
        
        # fit neural network
        NN_spring[[i]] = neuralnet(Drawn~., data = temp[temp$Season == "Spring",-3], hidden = hidden_nn,
                                   linear.output = T, stepmax = 1e7, lifesign = 'full')
        
      }
      
      ## Save the model and supporting files
      save(DISCO, NN_winter, NN_summer, NN_autumn, NN_spring, file = "DISCO_NN_model.RData")
      
    }
    
    # remove it when done
    remove_modal_gif()
    
    RV$DISCO_model_training = TRUE
    
  })
  
  
  #### Saving latest data
  observe({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    Historic_DISCO_Load <- tryCatch(download_files_from_google_drive("Historic_DISCO_Load.csv"), 
                                    error = function(e){NULL})
    
    if(is.null(Historic_DISCO_Load) | max(Historic_DISCO_Load$Time) != as.POSIXct(paste0(Sys.Date()-1, " 23:00:00 PKT"))){
      
      loading_screen("Accessing Lattest DISCO Data ... Please Wait")
      
      Live_PITC_DISCO_Load <- download_files_from_google_drive("Live_PITC_DISCO_Load.csv", fresh_download = TRUE)
      
      Historic_PITC_DISCO_Load <- download_files_from_google_drive("Historic_PITC_DISCO_Load.csv")
      
      Historic_DISCO_Load = rbind(Historic_PITC_DISCO_Load, Live_PITC_DISCO_Load)
      
      Historic_DISCO_Load = Historic_DISCO_Load[-which(duplicated(Historic_DISCO_Load, 
                                                                  by="Time")),]
      
      Historic_DISCO_Load = Historic_DISCO_Load[Historic_DISCO_Load$Time >= as.POSIXct("2020-12-01") &
                                                  Historic_DISCO_Load$Time < as.POSIXct(paste0(Sys.Date(), " PKT")), ]
      
      upload_files_to_google_drive("Historic_DISCO_Load.csv", Historic_DISCO_Load)
      
      # remove it when done
      remove_modal_gif()
      
    }
    
  })
  
  
  #### DISCOs Calculating New Forecast ####
  # observe({
  #   
  #   
  #   if(req(input$sidebar_tab_selected) == "home"){
  #     
  #     ## Requires that the user is logged in
  #     req(RV$Logged_in_status)
  #     
  #     req(RV$DISCO_model_training)
  #     
  #     loading_screen("Forecasting DISCO Load ... Please Wait")
  #     
  #     ## disable download of latest forecast data
  #     # disable('download_forecast_Data')
  #     
  #     if(Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT")))
  #       # if(TRUE)
  #     {
  #       
  #       
  #       cat("Calculating New Forecast \n")
  #       
  #       ## Call function to get latest forecast data frame
  #       PITC_Forecast_for_Tomorrow <- RV$PITC_Forecast_for_Tomorrow
  #       
  #       ## Call function to get lattest historic data.
  #       ## This will only get the data stored in Historic_DISCO_Load.csv
  #       Historic_DISCO_Load <- RV$Historic_DISCO_Load
  #       
  #       Historic_DISCO_Load = Historic_DISCO_Load[, c(1, grep("Drawn", colnames(Historic_DISCO_Load)))]
  #       
  #       
  #       PITC_Historic_Forecast <- RV$PITC_Historic_Forecast
  #       
  #       
  #       if(!is.null(PITC_Forecast_for_Tomorrow)){
  #         
  #         ## Check if the PITC_Forecast_for_Tomorrow.csv has the latest forecast i.e. for the next day
  #         if(PITC_Forecast_for_Tomorrow$Time[1] != as.POSIXct(paste0((Sys.Date()+1)," PKT"))){
  #           
  #           need_new_forecast = TRUE
  #           
  #         } else {
  #           
  #           need_new_forecast = FALSE
  #           
  #         }
  #         
  #       } else{
  #         
  #         need_new_forecast = TRUE
  #         
  #       }
  #       # need_new_forecast=FALSE
  #       
  #       if(need_new_forecast)
  #         # if(FALSE)
  #       {
  #         
  #         ## Load the .RData file which has our Neural network
  #         load("DISCO_NN_model.RData")
  #         
  #         
  #         
  #         ## Calculate the number of rows to add to historic data to create empty data until
  #         ## tomorrow for which the forecast is required.
  #         hour_difference = 24* as.vector(as.POSIXct(paste0((Sys.Date()+1)," 23:00:00 PKT"))-
  #                                           Historic_DISCO_Load$Time[nrow(Historic_DISCO_Load)])
  #         
  #         temp = data.frame(matrix(nrow = hour_difference, ncol = ncol(Historic_DISCO_Load)))
  #         colnames(temp) = colnames(Historic_DISCO_Load)
  #         
  #         ## Create extra rows with empty rows
  #         Historic_DISCO_Load = rbind(Historic_DISCO_Load, temp)
  #         
  #         ## Add time data to time rows
  #         Historic_DISCO_Load$Time =
  #           seq.POSIXt(from = Historic_DISCO_Load$Time[1],
  #                      to = as.POSIXct(paste0((Sys.Date()+1)," 23:00:00 PKT")), by = "hour")
  #         
  #         
  #         
  #         temp =  data.frame(sapply(Historic_DISCO_Load[,-1], function(x){shift(x, 168)}))
  #         
  #         colnames(temp) = paste0(colnames(temp),"_Lag168")
  #         
  #         Historic_DISCO_Load = cbind(Historic_DISCO_Load, temp)
  #         
  #         
  #         
  #         ## Filter data for the last 24 rows, which will be the day for which we need forecast
  #         Historic_DISCO_Load =
  #           Historic_DISCO_Load[(nrow(Historic_DISCO_Load)-23):nrow(Historic_DISCO_Load),]
  #         
  #         
  #         cat("Calculating NEW Forecast !!!")
  #         
  #         if(all(as.vector(!is.na(Historic_DISCO_Load[, grep("_Lag168", colnames(Historic_DISCO_Load))]))))
  #           # if(FALSE)
  #         {
  #           
  #           Season = get_season_data(Historic_DISCO_Load$Time[1],
  #                                    Historic_DISCO_Load$Time[nrow(Historic_DISCO_Load)])
  #           Historic_DISCO_Load = merge(Historic_DISCO_Load, Season, by = "Time")
  #           
  #           
  #           
  #           scaled_DISCO_Prediction = as.data.frame(Historic_DISCO_Load[,-1])
  #           
  #           cols <- colnames(scaled_DISCO_Prediction[, -which(colnames(scaled_DISCO_Prediction) %in% "Season")])
  #           
  #           # i = 1
  #           for ( i in 1:length(cols) ){
  #             
  #             max_NPCC = max(DISCO[,cols[i]])
  #             min_NPCC = min(DISCO[,cols[i]])
  #             
  #             scaled_DISCO_Prediction[, cols[i]] <- scale(scaled_DISCO_Prediction[,cols[i]],
  #                                                         center = min_NPCC,
  #                                                         scale = max_NPCC - min_NPCC)
  #           }
  #           
  #           scaled_DISCO_Prediction[,-which(colnames(scaled_DISCO_Prediction) %in% "Season")] =
  #             lapply(scaled_DISCO_Prediction[ ,-which(colnames(scaled_DISCO_Prediction) %in% "Season")], as.numeric)
  #           
  #           DISCOs_names = req(RV$DISCOs_names)
  #           
  #           New_Prediction = cbind(Time = Historic_DISCO_Load$Time,
  #                                  data.frame(matrix(nrow = 24, ncol = length(DISCOs_names))))
  #           
  #           colnames(New_Prediction)[-1] = DISCOs_names
  #           
  #           # i = 1
  #           for(i in 1:10){
  #             
  #             temp = scaled_DISCO_Prediction[,c(grep(DISCOs_names[i], colnames(scaled_DISCO_Prediction)),
  #                                               ncol(scaled_DISCO_Prediction))]
  #             
  #             colnames(temp) = c("Drawn", "Lag168", "Season")
  #             
  #             
  #             if(temp$Season[1] == "Winter"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_winter[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season[1] == "Summer"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_summer[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season == "Autumn"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_autumn[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season[1] == "Spring"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_spring[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             }
  #             
  #           }
  #           
  #           
  #           PITC_Historic_Forecast = merge(PITC_Historic_Forecast,
  #                                          PITC_Forecast_for_Tomorrow, all = TRUE)
  #           
  #           
  #           
  #           PITC_Forecast_for_Tomorrow = New_Prediction
  #           
  #           RV$PITC_Forecast_for_Tomorrow = PITC_Forecast_for_Tomorrow
  #           
  #           
  #           
  #           RV$PITC_Historic_Forecast = PITC_Historic_Forecast
  #           
  #           
  #           
  #           
  #           upload_files_to_google_drive("PITC_Forecast_for_Tomorrow.csv", PITC_Forecast_for_Tomorrow)
  #           
  #           upload_files_to_google_drive("PITC_Historic_Forecast.csv", PITC_Historic_Forecast)
  #           
  #           
  #           
  #           RV$DISCO_Forecast_status = paste("<p style='font-size:20px;'>The DISCOs Forecasts for",
  #                                            format((PITC_Forecast_for_Tomorrow$Time[1]), "%A %B %d, %Y"), "is Available !</p>")
  #           
  #           
  #           
  #         } else {
  #           ## If 48-hour old data is not available
  #           
  #           RV$DISCO_Forecast_status = paste("<p style='font-size:20px;'>The DISCOs Forecast for", format((Sys.Date()+1), "%A %B %d, %Y"),
  #                                            "is Not Available !</p>",
  #                                            "<br> <p style='font-size:20px;color:red;'> Make sure Data for",
  #                                            format((Sys.Date()-7), "%A %B %d, %Y"),"&", format((Sys.Date()-1), "%A %B %d, %Y"),
  #                                            "is uploaded Atleast</p>")
  #           
  #         }
  #         
  #       } else {
  #         
  #         ## The forecast for tomorrow is already calculated
  #         RV$DISCO_Forecast_status = paste("<p style='font-size:20px;'>The DISCOs Forecast for",
  #                                          format((PITC_Forecast_for_Tomorrow$Time[1]), "%A %B %d, %Y"), "is Available !</p>")
  #         
  #       }
  #       
  #     } else {
  #       
  #       ## forecast will be available after 10 am
  #       RV$DISCO_Forecast_status = paste("<p style='font-size:20px;'>The DISCOs Forecast for",
  #                                        format((Sys.Date()+1), "%A %B %d, %Y"), "will be available after 10 am</p>")
  #       
  #     }
  #     
  #     # remove it when done
  #     remove_modal_gif()
  #     
  #   }
  #   
  # })
  # 
  # 
  # #### DISCO Calculating Forecast for next 6 days ####
  # observe({
  #   
  #   if(req(input$sidebar_tab_selected) == "home"){
  #     
  #     ## Requires that the user is logged in
  #     req(RV$Logged_in_status)
  #     
  #     req(RV$DISCO_model_training)
  #     
  #     loading_screen("Forecasting 6 Day Load ... Please Wait")
  #     
  #     ## disable download of latest forecast data
  #     # disable('download_forecast_Data')
  #     
  #     if(Sys.time() > as.POSIXct(paste(Sys.Date(), "10:00:00 PKT"))){
  #       
  #       
  #       ## Call function to get lattest historic data.
  #       ## This will only get the data stored in Historic_DISCO_Load.csv
  #       Historic_DISCO_Load <- RV$Historic_DISCO_Load
  #       
  #       Historic_DISCO_Load = Historic_DISCO_Load[, c(1, grep("Drawn", colnames(Historic_DISCO_Load)))]
  #       
  #       DISCO_week_forecast <- RV$DISCO_week_forecast
  #       
  #       if(!any(is.null(as.vector(as.matrix(DISCO_week_forecast[,2:11]))))){
  #         
  #         ## Check if the DISCO_week_forecast.csv has the latest forecast i.e. for the next six days
  #         if(DISCO_week_forecast$Time[1] != as.POSIXct(paste(Sys.Date()+2, "PKT"))){
  #           
  #           DISCO_week_forecast = NULL
  #           
  #         }
  #         
  #       } else {
  #         
  #         DISCO_week_forecast = NULL
  #         
  #       }
  #       
  #       
  #       if(is.null(DISCO_week_forecast)){
  #         
  #         
  #         DISCOs_names = req(RV$DISCOs_names)
  #         
  #         DISCO_week_forecast = cbind(data.frame(Time = seq.POSIXt(from = as.POSIXct(paste(Sys.Date()+2, "PKT")),
  #                                                                  to = as.POSIXct(paste(Sys.Date()+7, "23:00:00 PKT")),
  #                                                                  by = "hour")), data.frame(matrix(nrow = 144, ncol = 10)))
  #         
  #         colnames(DISCO_week_forecast)[-1] = DISCOs_names
  #         
  #         
  #         
  #         Historic_DISCO_Load = merge(Historic_DISCO_Load, DISCO_week_forecast, all = TRUE)
  #         
  #         temp =  data.frame(sapply(Historic_DISCO_Load[,-1], function(x){shift(x, 168)}))
  #         
  #         colnames(temp) = paste0(colnames(temp),"_Lag168")
  #         
  #         Historic_DISCO_Load = cbind(Historic_DISCO_Load, temp)
  #         
  #         DISCO_week_forecast =  Historic_DISCO_Load[Historic_DISCO_Load$Time >= min(DISCO_week_forecast$Time) &
  #                                                      Historic_DISCO_Load$Time <= max(DISCO_week_forecast$Time), c(1,12:31)]
  #         
  #         
  #         
  #         
  #         ## Load the .RData file which has our Neural network
  #         load("DISCO_NN_model.RData")
  #         
  #         if(all(as.vector(!is.na(DISCO_week_forecast[, grep("_Lag168", colnames(DISCO_week_forecast))]))))
  #           # if(FALSE)
  #         {
  #           
  #           Season = get_season_data(DISCO_week_forecast$Time[1],
  #                                    DISCO_week_forecast$Time[nrow(DISCO_week_forecast)])
  #           DISCO_week_forecast = merge(DISCO_week_forecast, Season, by = "Time")
  #           
  #           
  #           
  #           scaled_DISCO_Prediction = as.data.frame(DISCO_week_forecast[,-1])
  #           
  #           cols <- colnames(scaled_DISCO_Prediction[, -which(colnames(scaled_DISCO_Prediction) %in% c(DISCOs_names, "Season"))])
  #           
  #           # i = 1
  #           for ( i in 1:length(cols) ){
  #             
  #             max_NPCC = max(DISCO[,cols[i]])
  #             min_NPCC = min(DISCO[,cols[i]])
  #             
  #             scaled_DISCO_Prediction[, cols[i]] <- scale(scaled_DISCO_Prediction[,cols[i]],
  #                                                         center = min_NPCC,
  #                                                         scale = max_NPCC - min_NPCC)
  #           }
  #           
  #           scaled_DISCO_Prediction[,-which(colnames(scaled_DISCO_Prediction) %in% "Season")] =
  #             lapply(scaled_DISCO_Prediction[ ,-which(colnames(scaled_DISCO_Prediction) %in% "Season")], as.numeric)
  #           
  #           DISCOs_names = c("LESCO", "MEPCO", "FESCO", "QESCO",
  #                            "PESCO", "GEPCO", "HESCO", "SEPCO",
  #                            "IESCO", "TESCO")
  #           
  #           New_Prediction = cbind(Time = DISCO_week_forecast$Time,
  #                                  data.frame(matrix(nrow = 24, ncol = length(DISCOs_names))))
  #           
  #           colnames(New_Prediction)[-1] = DISCOs_names
  #           
  #           # i = 1
  #           for(i in 1:10){
  #             
  #             temp = scaled_DISCO_Prediction[,c(grep(DISCOs_names[i], colnames(scaled_DISCO_Prediction)),
  #                                               ncol(scaled_DISCO_Prediction))]
  #             
  #             colnames(temp) = c("Drawn", "Lag168", "Season")
  #             
  #             
  #             if(temp$Season[1] == "Winter"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_winter[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season[1] == "Summer"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_summer[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season == "Autumn"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_autumn[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             } else if(temp$Season[1] == "Spring"){
  #               
  #               New_Prediction[[DISCOs_names[i]]] =
  #                 (neuralnet::compute(NN_spring[[i]], temp[,-1])$net.result *
  #                    (max(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T) -
  #                       min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T))) +
  #                 min(DISCO[[paste0("Drawn_", DISCOs_names[i])]], na.rm = T)
  #               
  #             }
  #             
  #           }
  #           
  #           
  #           DISCO_week_forecast = New_Prediction
  #           RV$DISCO_week_forecast = DISCO_week_forecast
  #           
  #         }
  #         
  #         upload_files_to_google_drive("DISCO_week_forecast.csv", DISCO_week_forecast)
  #         
  #       }
  #       
  #     }
  #     
  #   }
  #   
  #   # remove it when done
  #   remove_modal_gif()
  #   
  # })
  # 
  
  #### Announce that new forecast is available or not ####
  output$DISCO_forecast_info = renderUI({ HTML(RV$DISCO_Forecast_status) })
  
  
  ##### Data Start - End Date #####
  output$DISCO_date_range_home_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("DISCO_date_range_home", "Select Date Range",
                   start = Sys.Date()+1,
                   end = Sys.Date()+1,
                   min = Sys.Date()+1,
                   max = Sys.Date()+7)
    
  })
  
  
  ## Select disco for forecast plot
  output$DISCO_home_tab_DISCO_selection_UI = renderUI({
    
    req(RV$PITC_Forecast_for_Tomorrow)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    profile_name <- RV$profiles$Username[req(RV$profiles_index)]
    
    if(any(DISCOs_names == profile_name)){ DISCOs_names = profile_name}
    
    pickerInput(inputId = "DISCO_home_tab_DISCO_selection", 
                label = "Select DISCO",
                selected = DISCOs_names[1],
                choices = DISCOs_names,
                options = pickerOptions(dropupAuto = FALSE, `actions-box` = TRUE ),
                multiple = FALSE)
    
  })
  
  
  
  ## manage DISCO plot tabs
  output$Home_DISCO_tabs = renderUI({
    
    if(req(RV$Logged_in_status)){
      
      DISCOs_names = req(RV$DISCOs_names)
      
      selected_DISCO = req(input$DISCO_home_tab_DISCO_selection)
      
      DISCO_predicion_plot = req(RV$DISCO_predicion_plot)
      
      req(input$sidebar_tab_selected)
      
      Exclusions <- unlist(strsplit(
        as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
      
      req(input$sidebar_tab_selected)
      
      loading_screen("Please Wait")
      
      DISCO_tabs = do.call(tabsetPanel, c(lapply(1:length(selected_DISCO), function(i) {
        tabPanel(
          title = selected_DISCO, 
          
          DISCO_predicion_plot[[i]]
          
        )
      })))
      
      # remove it when done
      remove_modal_gif()
      
      DISCO_tabs
      
    }
    
  })
  
  
  #### Forecast Plot ####
  observe({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Preparing DISCO Data ... Please Wait")
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    selected_DISCO = req(input$DISCO_home_tab_DISCO_selection)
    
    DISCO_index <- which(DISCOs_names %in% selected_DISCO)
    
    DISCO_date_range_home <- req(input$DISCO_date_range_home)
    
    ## Get lattest forecast data frame
    Forecast_for_Tomorrow <- rbind(req(RV$PITC_Forecast_for_Tomorrow), req(RV$DISCO_week_forecast))
    
    Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(DISCO_date_range_home)," 00:00:00 PKT")) &
                                                     Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(DISCO_date_range_home)," 23:00:00 PKT")),]
    
    
    if(nrow(Forecast_for_Tomorrow) > 0){
      
      DISCO_predicion_plot = list()
      
      # i = 1
      for(i in DISCO_index){ 
        
        ## Line plot of Predicted load
        DISCO_predicion_plot[[1]] = plot_ly(data = Forecast_for_Tomorrow, x=~Time)%>%
          add_trace(y= Forecast_for_Tomorrow[[DISCOs_names[i]]], name = "Forecast[MW]", type = "scatter",
                    mode = "lines",color = I('red'), fill = "tozeroy",
                    hoverinfo = "text",
                    hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                       "<br> Predicted Load :", 
                                       round(Forecast_for_Tomorrow[[DISCOs_names[i]]],2), "MW"))%>%
          layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ),
                 yaxis = list( title = paste0(DISCOs_names[i], " Load [MW]")))
        
        
      }
      
      RV$DISCO_predicion_plot = DISCO_predicion_plot
      
    }
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  
  
  ## Select disco for data download
  output$DISCO_tab_download_selection_UI = renderUI({
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    profile_name <- RV$profiles$Username[req(RV$profiles_index)]
    
    if(any(DISCOs_names == profile_name)){ DISCOs_names = profile_name}
    
    pickerInput(inputId = "DISCO_tab_download_selection", 
                label = "Select DISCOs",
                selected = DISCOs_names,
                choices = DISCOs_names,
                options = pickerOptions(dropupAuto = FALSE, `actions-box` = TRUE ),
                multiple = TRUE)
    
  })
  
  
  #### DISC Forecast Download Handler ####
  output$DISCO_download_forecast_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("DISCO-Forecast-",
                                 as.character(min(input$DISCO_date_range_home)),"-", 
                                 as.character(max(input$DISCO_date_range_home)), 
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      DISCOs_names <- req(RV$DISCOs_names)
      
      DISCO_date_range_home <- req(input$DISCO_date_range_home)
      
      DISCO_tab_download_selection = req(input$DISCO_tab_download_selection)
      
      ## Get lattest forecast data frame
      Forecast_for_Tomorrow <- rbind(req(RV$PITC_Forecast_for_Tomorrow), req(RV$DISCO_week_forecast))
      
      Forecast_for_Tomorrow <- Forecast_for_Tomorrow[Forecast_for_Tomorrow$Time >= as.POSIXct(paste0(min(DISCO_date_range_home)," 00:00:00 PKT")) &
                                                       Forecast_for_Tomorrow$Time <= as.POSIXct(paste0(max(DISCO_date_range_home)," 23:00:00 PKT")), c("Time", DISCO_tab_download_selection)]
      
      log_entry(req(RV$Username), "Downloaded DISCOs Forecast")
      
      
      write.csv(Forecast_for_Tomorrow, file, row.names = FALSE)
      
      
    })
  
  
  
  
  
  
  ############### Upload New Data ######################
  
  ## Dont even show The save new data button, until data is ready
  hide("save_new_data_sample_file")
  
  observe({
    
    ## Get Historic data file, this only gets Historic_NPCC_Load_Forecast_Weather.csv
    ## NOT the Historic_NPCC_Load_Forecast_Weather_OLD.csv
    Historic_NPCC_Load_Forecast_Weather <- req(RV$Historic_NPCC_Load_Forecast_Weather)
    
    new_data_required_date =
      Historic_NPCC_Load_Forecast_Weather$Time[min(which(is.na(Historic_NPCC_Load_Forecast_Weather$hourly_load)))]
    
    if(is.na(new_data_required_date)){
      
      new_data_required_date = 
        Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)]
      
    }
    
    RV$new_data_required_date = as.Date(new_data_required_date) + 1
    
    
  })
  
  ## Display message to show the data missing date
  output$data_required_date_message = renderUI({
    
    HTML(paste("<p style='font-size:20px;'>Data is required from", 
               format(req(RV$new_data_required_date), "%A %B %d, %Y"), 
               "</p>"))
  })
  
  ##### Sample Data Start Date #####
  output$start_date_UI_sample_file <- renderUI({
    
    ## Start data for the sample data
    dateInput("start_date_sample_file", "Start Date:", 
              value = req(RV$new_data_required_date))
    
  })
  
  ##### Sample Data End Date #####
  output$end_date_UI_sample_file <- renderUI({
    
    ## End date for the sample data
    dateInput("end_date_sample_file", "End Date:", value = Sys.Date()-1)
    
  })
  
  ##### Sample New Data .xlsx File Download Handler #####
  observe({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## This sample file will help user to create a new data file as is required by the tool.
    output$download_sample_file <- downloadHandler(
      
      ## Evaluate the name of the sample file
      filename = function() {
        paste("NPCC-Load-",as.character(input$start_date_sample_file),
              "-to-",as.character(input$end_date_sample_file),".xlsx", sep = "")
        # paste("NPCC-Load-",as.character(input$start_date_sample_file),
        #       "-to-",as.character(input$end_date_sample_file),".csv", sep = "")
      },
      
      ## Function to invoke download of sample file on internet browser
      content = function(file) {
        
        from_date <- input$start_date_sample_file
        to_date <- input$end_date_sample_file
        
        
        temp = data.frame(matrix(nrow = (as.numeric(as.Date(to_date) - as.Date(from_date))+1), ncol = 25))
        
        temp[,1] = seq.Date(from = from_date, to = to_date, by = "day")
        colnames(temp) = c("",1:24)
        
        write.xlsx(temp, file)
        # write.csv(temp, file, row.names = FALSE)
        
      })
  })
  
  ###### New Data Table #####
  output$new_data_table_sample_file <- renderDataTable({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Until the user upload new data file, nothing in this function runs
    req(input$new_data_input_sample_file)
    
    ## Make sure, if the user uploads wrong file the shiny app does not fail 
    tryCatch(
      {
        ## Read the excel file
        new_data <- read.xlsx(input$new_data_input_sample_file$datapath, detectDates = TRUE)
        # new_data <- read.csv(input$new_data_input_sample_file$datapath, detectDates = TRUE)
        
        temp = data.frame(Time = seq.POSIXt(from = as.POSIXct(paste0(new_data[1,1], " PKT")),
                                            to = as.POSIXct(paste0(new_data[nrow(new_data),1], " 23:00:00 PKT")),
                                            by = "hour"),
                          hourly_load = NA)
        
        temp$hourly_load = melt(t(new_data[,-1]))[,3]
        
        new_data = temp
        
      },
      error = function(e) {
        
        ## return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    ## Convert Time column to posixct
    new_data$Time = as.POSIXct(new_data$Time)
    
    ## Save the newly uploaded data to the reactive object's data frame 
    ## to invoke other functions to be re-evaluated 
    RV$upload_data_tab_new_data = new_data
    
    ## Display the save button, now that data is ready to be saved
    shinyjs::show("save_new_data_sample_file")
    
    
    
    
    Days = unique(substr(as.character(new_data$Time), 1, 10))
    
    new_data$Days = rep(Days, each = 24)
    
    new_data$Hour = 1:24
    
    
    Formatted_historic_data = data.frame( matrix(nrow = (length(Days)), ncol = 25 ) )
    
    Formatted_historic_data$X1 = Days
    
    
    # hour_index = 1
    # day_index = 1
    
    for(hour_index in 1:24){
      
      for (day_index in 1:length(Days)) {
        
        Formatted_historic_data[Formatted_historic_data$X1 == Days[day_index],(hour_index+1)] = as.numeric(new_data[new_data$Days == Days[day_index] & 
                                                                                                                      new_data$Hour == hour_index, 2])
      }
    }
    
    colnames(Formatted_historic_data) = c("Day / Hour", paste0("0",1:9,"00"), paste0(10:24,"00"))
    
    ## Display the newly uploaded data
    datatable({
      
      
      Formatted_historic_data
      
    }, options = list(scrollX = TRUE, stateSave = TRUE, pageLength = 100), rownames= FALSE)
    
  })
  
  
  ##### Saving New Data #####
  observeEvent(input$save_new_data_sample_file,{
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Saving New Data ... Please Wait")
    
    
    ## Retrieve the newly uploaded data
    new_data <- req(RV$upload_data_tab_new_data)
    
    ## Get Historic data file, this only gets Historic_NPCC_Load_Forecast_Weather.csv
    ## NOT the Historic_NPCC_Load_Forecast_Weather_OLD.csv
    Historic_NPCC_Load_Forecast_Weather <- req(RV$Historic_NPCC_Load_Forecast_Weather)
    
    column_names = colnames(Historic_NPCC_Load_Forecast_Weather)
    
    ## Change progress tracker
    # setProgress(0.2, detail = "Accessing Historic Data")
    
    ## Check if The new data is not empty
    if(colnames(new_data)[1] == "Time" &
       colnames(new_data)[2] == "hourly_load" &
       !is.null(as.POSIXct(new_data[,1])) &
       is.numeric(new_data[,2])){
      
      # if(FALSE){
      
      ## add time rows to the historic data frame
      Historic_NPCC_Load_Forecast_Weather = merge(Historic_NPCC_Load_Forecast_Weather, 
                                                  data.frame(Time = new_data$Time), all = TRUE )
      
      ## Add the hourly_load data to historic data frame
      Historic_NPCC_Load_Forecast_Weather$hourly_load[which(
        Historic_NPCC_Load_Forecast_Weather$Time %in% new_data$Time)] = new_data$hourly_load
      
      
      ## Get Weather Data
      {
        
        Cities <- RV$Cities
        
        ## dates for which weather data is required
        dates = unique(substr(as.character(new_data$Time), 1, 10))
        
        ## Initialize dataframe that will contain weather data
        Cities_apparent_temperature = data.frame(Time = new_data$Time)
        
        ## loop for each city
        for(i in 1:nrow(Cities)){
          
          temp = NULL
          
          ## loop for each date
          for(j in 1:length(dates)){
            
            cat("\014")
            cat("Getting data for", Cities$City[i], dates[j])
            
            ## weather data for each city for each day
            weather_data = get_weather_data(dates[j], Cities$Lat[i], Cities$Lng[i])
            temp = rbind(temp,  weather_data)
            
          }
          
          ## Change name of column
          colnames(temp)[2:ncol(temp)] = paste0(Cities$City[i],"_", colnames(temp)[2:ncol(temp)])
          
          ## merge apparent temperature data for each city
          Cities_apparent_temperature = merge(Cities_apparent_temperature, temp, by = "Time")
          
        }
        
        New_Data = merge(Historic_NPCC_Load_Forecast_Weather[Historic_NPCC_Load_Forecast_Weather$Time == new_data$Time, c(1,2,3)],
                         Cities_apparent_temperature)
        
        
        Historic_NPCC_Load_Forecast_Weather[Historic_NPCC_Load_Forecast_Weather$Time == new_data$Time,] = New_Data
        
      }
      
      
      ## Sort rows 
      Historic_NPCC_Load_Forecast_Weather = 
        Historic_NPCC_Load_Forecast_Weather[order(Historic_NPCC_Load_Forecast_Weather$Time),]
      
      ## Sort Columns
      Historic_NPCC_Load_Forecast_Weather = Historic_NPCC_Load_Forecast_Weather[,column_names]
      
      
      
      RV$Historic_NPCC_Load_Forecast_Weather <- Historic_NPCC_Load_Forecast_Weather
      
      upload_files_to_google_drive("Historic_NPCC_Load_Forecast_Weather.csv", 
                                   Historic_NPCC_Load_Forecast_Weather)
      
      
      log_entry(req(RV$Username), "Uploaded New Data")
      
      #### generate message
      showNotification("New Data Updated !", type = "message")
      
      
    } else {
      
      #### generate message
      showNotification("Uploaded file does not have correct format !", type = "error")
      
    }
    
    # })
    
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  
  ##### Data Uploaded Table ######
  output$Uploaded_data_table = renderDataTable({
    
    Historic_NPCC_Load_Forecast_Weather <- req(RV$Historic_NPCC_Load_Forecast_Weather)
    
    Historic_NPCC_Load_Forecast_Weather = Historic_NPCC_Load_Forecast_Weather[Historic_NPCC_Load_Forecast_Weather$Time >= as.POSIXct(paste0(Sys.Date()-10, " 00:00:00 PKT")),c(1,2)]
    
    
    Historic_NPCC_Load_Forecast_Weather = rbind(Historic_NPCC_Load_Forecast_Weather, 
                                                data.frame(Time = seq.POSIXt(from = as.POSIXct(paste0(as.Date(Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)])+1, " 00:00:00 PKT")), 
                                                                             to = as.POSIXct(paste0(as.Date(Historic_NPCC_Load_Forecast_Weather$Time[nrow(Historic_NPCC_Load_Forecast_Weather)])+3, " 23:00:00 PKT")),
                                                                             by = "hour"),
                                                           hourly_load = NA))
    
    Days = unique(substr(as.character(Historic_NPCC_Load_Forecast_Weather$Time), 1, 10))
    
    Historic_NPCC_Load_Forecast_Weather$Days = rep(Days, each = 24)
    
    Historic_NPCC_Load_Forecast_Weather$Hour = 1:24
    
    
    Formatted_historic_data = data.frame( matrix(nrow = (length(Days)), ncol = 25 ) )
    
    Formatted_historic_data$X1 = Days
    
    
    # hour_index = 1
    # day_index = 1
    
    for(hour_index in 1:24){
      
      for (day_index in 1:length(Days)) {
        
        Formatted_historic_data[Formatted_historic_data$X1 == Days[day_index],(hour_index+1)] = as.numeric(Historic_NPCC_Load_Forecast_Weather[Historic_NPCC_Load_Forecast_Weather$Days == Days[day_index] & 
                                                                                                                                                 Historic_NPCC_Load_Forecast_Weather$Hour == hour_index, 2])
      }
    }
    
    colnames(Formatted_historic_data) = c("Day / Hour", paste0("0",1:9,"00"), paste0(10:24,"00"))
    
    ## Display the newly uploaded data
    datatable({
      
      Formatted_historic_data
      
      
    }, options = list(scrollX = TRUE, stateSave = TRUE, pageLength = 100), rownames= FALSE)
    
  })
  
  
  
  ######## DISCO #########
  
  ##### Data Start - End Date #####
  output$DISCO_date_range_upload_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateInput("DISCO_date_range_upload", "Select Date", 
              value = Sys.Date()+1,
              min = "2016-07-01",
              max = Sys.Date()+1)
    
  })
  
  observe({
    
    req(RV$Logged_in_status)
    
    loading_screen("Preparing Load Maintenance DISCO Data ... Please Wait")
    
    All_DISCO_Load_Maintenance <- req(RV$All_DISCO_Load_Maintenance)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    profile_name <- RV$profiles$Username[req(RV$profiles_index)]
    
    Exclusions <- unlist(strsplit(
      as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
    
    Date_range <- req(input$DISCO_date_range_upload)
    
    
    
    if(!is.null(All_DISCO_Load_Maintenance)){
      
      if(max(All_DISCO_Load_Maintenance$Time) != as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT"))){
        
        All_DISCO_Load_Maintenance <- merge(All_DISCO_Load_Maintenance, 
                                            data.frame(Time = seq.POSIXt(from = as.POSIXct(paste0(min(Date_range), " 00:00:00 PKT")), 
                                                                         to = as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT")), 
                                                                         by = "hour")), all = TRUE)
      }
      
    }
    
    
    DISCO_Load_Maintenance = All_DISCO_Load_Maintenance[All_DISCO_Load_Maintenance$Time >= as.POSIXct(paste0(min(Date_range), " 00:00:00 PKT")) &
                                                          All_DISCO_Load_Maintenance$Time <=  as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT")) , ] 
    
    if(any(DISCOs_names == profile_name)){
      
      DISCO_Load_Maintenance = DISCO_Load_Maintenance[ , c("Time", profile_name)]
      
    }
    
    RV$DISCO_Load_Maintenance = DISCO_Load_Maintenance
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  options(DT.options = list(pageLength = 24))
  
  
  output$DISCO_Uploaded_data_table = renderDataTable({
    
    DISCO_Load_Maintenance <- req(RV$DISCO_Load_Maintenance)
    
    DISCO_Load_Maintenance$Time = paste0(format(DISCO_Load_Maintenance$Time, "%d-%b-%Y"), " Hour: ",
                                         as.numeric(format(DISCO_Load_Maintenance$Time, "%H"))+1)
    
    datatable(DISCO_Load_Maintenance, 
              selection = 'none', 
              editable = list(target = 'cell', 
                              disable = list(columns = c(0))),
              rownames = FALSE)
    
    
  })
  
  
  observeEvent(input$DISCO_Uploaded_data_table_cell_edit, {
    
    Editted_data = RV$Editted_data
    
    Editted_data <- rbind(Editted_data, input$DISCO_Uploaded_data_table_cell_edit)
    
    RV$Editted_data = Editted_data
    
  })
  
  
  observeEvent(input$DISCO_save_new_data_sample_file, {
    
    loading_screen("Saving Load Maintenance DISCO Data ... Please Wait")
    
    DISCO_Load_Maintenance <- req(RV$DISCO_Load_Maintenance)
    
    Edit_Disco_maintenance_info <- req(RV$Editted_data)
    
    Date_range <- req(input$DISCO_date_range_upload)
    
    All_DISCO_Load_Maintenance <- tryCatch(download_files_from_google_drive("DISCO_Load_Maintenance.csv", 
                                                                            fresh_download = TRUE), 
                                           error = function(e){NULL})
    
    
    Edit_Disco_maintenance_info$row = unique(Edit_Disco_maintenance_info$row, fromLast = TRUE)
    
    vec = Edit_Disco_maintenance_info$row
    
    if(any(duplicated(vec, fromLast = TRUE))){
      
      Edit_Disco_maintenance_info = Edit_Disco_maintenance_info[-which(duplicated(vec, fromLast = TRUE)),]
      # Edit_Disco_maintenance_info = Edit_Disco_maintenance_info[Edit_Disco_maintenance_info$value != "",]
      
    }
    
    if(nrow(Edit_Disco_maintenance_info) > 0){
      
      Edit_Disco_maintenance_info$value = ifelse(Edit_Disco_maintenance_info$value == "",NA, Edit_Disco_maintenance_info$value)
      
      DISCO_Load_Maintenance[Edit_Disco_maintenance_info$row, 
                             Edit_Disco_maintenance_info$col[1]+1] = Edit_Disco_maintenance_info$value
      
    }
    
    if(!is.null(All_DISCO_Load_Maintenance)){
      
      if(max(All_DISCO_Load_Maintenance$Time) != as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT"))){
        
        All_DISCO_Load_Maintenance <- merge(All_DISCO_Load_Maintenance, 
                                            data.frame(Time = seq.POSIXt(from = as.POSIXct(paste0(min(Date_range), " 00:00:00 PKT")), 
                                                                         to = as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT")), 
                                                                         by = "hour")), all = TRUE)
      }
      
      
      
      All_DISCO_Load_Maintenance[All_DISCO_Load_Maintenance$Time >= as.POSIXct(paste0(min(Date_range), " 00:00:00 PKT")) &
                                   All_DISCO_Load_Maintenance$Time <=  as.POSIXct(paste0(max(Date_range), " 23:00:00 PKT")) , 
                                 colnames(DISCO_Load_Maintenance)[2]] = DISCO_Load_Maintenance[,2]
      
      
      RV$DISCO_Load_Maintenance <- DISCO_Load_Maintenance
      
      RV$All_DISCO_Load_Maintenance = All_DISCO_Load_Maintenance
      
      
      upload_files_to_google_drive("DISCO_Load_Maintenance.csv",
                                   All_DISCO_Load_Maintenance)
      
    }
    
    # remove it when done
    remove_modal_gif()
    
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "Updated",
      type = "success"
    )
    
  })
  
  
  
  
  
  
  
  
  
  
  ############## Historic Data ################
  
  ##### Data Start - End Date #####
  output$date_range_historic_data_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("date_range_historic_data", "Select Date Range", 
                   start = Sys.Date()-14,
                   end = Sys.Date()-1,
                   min = "2016-07-01",
                   max = Sys.Date())
    
  })
  
  
  ##### Get and Filter Historic Data ####
  observeEvent(input$historic_data_plot_button,{
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Getting Historic Data ... Please Wait")
    
    Historic_NPCC_Load_Forecast_Weather = req(RV$Historic_NPCC_Load_Forecast_Weather)
    
    Historic_NPCC_Load_Forecast_Weather_OLD = 
      download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
    
    ## Convert Time to posixct
    Historic_NPCC_Load_Forecast_Weather_OLD$Time = as.POSIXct(Historic_NPCC_Load_Forecast_Weather_OLD$Time)
    
    ## Row bind both Old historic data and historic data
    historic_data_tab_data = rbind(Historic_NPCC_Load_Forecast_Weather_OLD, 
                                   Historic_NPCC_Load_Forecast_Weather)
    
    ## Add Historic data to historic_data_plot_reactive() after filtering
    RV$historic_data_tab_data = historic_data_tab_data[historic_data_tab_data$Time >= as.POSIXct(paste0(min(input$date_range_historic_data)," 00:00:00 PKT")) & 
                                                         historic_data_tab_data$Time <= as.POSIXct(paste0(max(input$date_range_historic_data)," 23:00:00 PKT")),]
    
    # remove it when done
    remove_modal_gif()
    
    ## Show the download historic data button
    # shinyjs::show("download_Historic_Data_div")
    
  })
  
  
  ##### Historic Line Plot #####
  output$historic_data_line_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait for historic_data_plot_reactive() to get data
    historic_data_tab_data = req(RV$historic_data_tab_data)
    
    ## Line plot for hourly_load
    plot_ly()%>%
      add_trace(data = historic_data_tab_data,
                x=~Time, y=~hourly_load, type = "scatter", mode = "lines", fill = "tozeroy",
                hoverinfo = "text", 
                hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                   "<br> Load :", round(hourly_load,2), "MW"))%>% 
      layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
             yaxis = list( title = "Load [MW]"))%>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(
               filename = paste0("Historic Line Plot ", min(input$date_range_historic_data), "-", max(input$date_range_historic_data)),
               format = "png",
               width = 900,
               height = 600
             ))
    
  })
  
  
  ##### Historic Pie Plot ######
  output$historic_data_pie_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait fir historic_data_plot_reactive()
    historic_data_tab_data = req(RV$historic_data_tab_data)
    
    ## Get the filtered data
    df_pie_plot <- historic_data_tab_data
    
    ## Make sure there are no NAs
    df_pie_plot = df_pie_plot[complete.cases(df_pie_plot$hourly_load),]
    
    ## Extract the weekdays from the time column
    df_pie_plot$Weekday = weekdays(df_pie_plot$Time)
    
    ## Calculate average for each weekday
    df_pie_plot = aggregate(df_pie_plot$hourly_load, list(df_pie_plot$Weekday), sum)
    
    ## Change column names
    colnames(df_pie_plot) = c("Day", "Energy")
    
    Days_vector = weekdays(as.Date("2020-07-05")+(1:7))
    
    df_pie_plot = df_pie_plot[match(Days_vector,df_pie_plot$Day),]
    
    ## Pie chart for weekdays average load
    plot_ly(df_pie_plot, labels = ~Day, values = ~Energy, 
            type = 'pie',textinfo = 'percent', 
            hoverinfo = 'text',
            sort = FALSE,
            direction = "clockwise",
            text = ~paste(Day,":",round(Energy/1000,2),"GWh"))%>%
      layout(title = list(text =  "Energy Distribution Among Weekdays", y = 1, xref = "paper", x = 2),
             legend=list(title=list(text=paste("<b>Total Energy:", 
                                               round(sum(df_pie_plot$Energy)/1000,2), "GWh<b>"))))%>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(
               filename = paste0("Historic Pie Chart ", 
                                 min(isolate(input$date_range_historic_data)), "-", 
                                 max(isolate(input$date_range_historic_data))),
               format = "png",
               width = 900,
               height = 600
             ))
  })
  
  
  ##### Historic Hourly Profile Plot ######
  output$historic_data_Hourly_boxplot_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Do not run the rest of the function until historic_data_plot_reactive() is NULL
    df_hourly_plot <- req(RV$historic_data_tab_data)
    
    ## Make sure there are no NAs
    df_hourly_plot = df_hourly_plot[complete.cases(df_hourly_plot$hourly_load),c("Time", "hourly_load")]
    
    ## Get the hour for each row
    df_hourly_plot$Hour = sub("^0+", "", format(df_hourly_plot$Time, "%I %p"))
    
    df_hourly_plot$Hour = factor( df_hourly_plot$Hour, levels = df_hourly_plot$Hour[1:24])
    
    
    plot_ly(df_hourly_plot, y = ~hourly_load, 
            x = ~Hour, type = "box", hoverinfo = 'y')%>%
      layout(yaxis = list(title = "Load [MW]"))%>%
      config(displaylogo = FALSE,
             toImageButtonOptions = list(
               filename = paste0("Historic Boxplot ", 
                                 min(isolate(input$date_range_historic_data)), "-", 
                                 max(isolate(input$date_range_historic_data))),
               format = "png",
               width = 900,
               height = 600
             ))
    
  })
  
  
  ##### Historic Stats Calculation ####
  output$historic_data_stats <- renderUI({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait until some data is recieved in historic_data_plot_reactive()
    historic_data_tab_data = req(RV$historic_data_tab_data)
    
    ## Get the filtered data
    df_stats_metrics <- historic_data_tab_data
    
    ## Make sure there are no NAs
    df_stats_metrics = df_stats_metrics[complete.cases(df_stats_metrics$hourly_load),]
    
    ## Calculate stats
    HTML(paste("<p style='font-size:20px;'>Energy:",round(sum(df_stats_metrics$hourly_load, na.rm = TRUE)/1000,2),"GWh",
               "<br>Average Power:",round(mean(df_stats_metrics$hourly_load, na.rm = TRUE),2), "MW",
               "<br>Min:",round(min(df_stats_metrics$hourly_load, na.rm = TRUE),2), "MW",
               "<br>Max:",round(max(df_stats_metrics$hourly_load, na.rm = TRUE),2), "MW",
               "<br>Load Factor:",round((sum(df_stats_metrics$hourly_load, na.rm = TRUE) / 
                                           (max(df_stats_metrics$hourly_load, na.rm = TRUE) * 
                                              nrow(df_stats_metrics))),2), "",
               "</p>"))
    
  })
  
  
  ##### Historic Heat Map ####
  output$historic_heatmap_plotly <- renderPlotly({ 
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait until some data is recieved in historic_data_plot_reactive()
    historic_data_tab_data <- req(RV$historic_data_tab_data)
    
    ## Check if data has data
    if (length(nrow(historic_data_tab_data)) != 0) {
      
      ## Make sure there are no NAs
      historic_usage = historic_data_tab_data[complete.cases(historic_data_tab_data$hourly_load),c("Time", "hourly_load")]
      
      ## Convert all columns to positive
      historic_usage[,-1] = abs(historic_usage[,-1])
      
      ## Get the hour for each row
      historic_usage$Hour = as.factor((as.POSIXlt(historic_usage$Time)$h)+1)
      
      ## Add day column
      historic_usage$Day = as.factor(weekdays(historic_usage$Time))
      
      ## only keep required columns
      historic_usage = historic_usage[,c("Hour", "Day", "hourly_load")]
      
      ## Get mean for each day of the week
      df_plot <- reshape2::dcast(data = historic_usage, Day~Hour, 
                                 fun.aggregate = mean,
                                 fill = 0,
                                 value.var = colnames(historic_usage)[3])
      
      ## Make weekdays, rowname of the dataframe
      row.names(df_plot) = df_plot$Day
      
      ## This variable helps in ordering the rows
      days.of.week <- as.factor(c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                  "Friday", "Saturday", "Sunday"))
      
      ## Ordering rows in dataframe
      df_plot = df_plot[order(df_plot$Day)[days.of.week], -1]
      
      ## Text to show on hovering
      Hover_text = matrix(nrow = nrow(df_plot), ncol = ncol(df_plot))
      
      x = seq.POSIXt(from = as.POSIXct("2020-09-07"), 
                     to = as.POSIXct("2020-09-07 23:00:00"), 
                     by = "hour")
      
      x = sub("^0+", "", format(x, "%I %p"))
      
      for(Day in 1:nrow(Hover_text)){
        
        for (Hour in 1:ncol(Hover_text)) {
          
          Hover_text[Day,Hour] = paste0("Day: ", row.names(df_plot)[Day], "\n",
                                        "Hour: ", x[Hour], "\n",
                                        "Power: ", round(df_plot[Day, Hour], 2), " MW")
          
        }
        
      }
      
      plot_ly(z = as.matrix(df_plot), y = rownames(df_plot), x = x, 
              colors = colorRamp(c("white", "blue")), type = "heatmap",
              hoverinfo='text',
              text= Hover_text,
              colorbar = list(title = "<b>Power [MW]</b>"))%>%
        layout(xaxis = list(title = "<b>Hour</b>"), 
               yaxis = list(title = "<b>Day</b>"))%>%
        config(displaylogo = FALSE,
               toImageButtonOptions = list(
                 filename = paste0("Historic Heat Map ", 
                                   min(isolate(input$date_range_historic_data)), "-", 
                                   max(isolate(input$date_range_historic_data))),
                 format = "png",
                 width = 900,
                 height = 600
               ))
      
    }
    
    
  })
  
  
  ##### Historic Load duration curve ####
  output$historic_LDC_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait until some data is recieved in historic_data_plot_reactive()
    historic_data_tab_data <- req(RV$historic_data_tab_data)
    
    ## Check if data has data
    if (length(nrow(historic_data_tab_data)) != 0) {
      
      ## Make sure there are no NAs
      historic_usage <- historic_data_tab_data[complete.cases(historic_data_tab_data$hourly_load),c("Time", "hourly_load")]
      
      ## Convert all columns to positive
      historic_usage[,-1] = abs(historic_usage[,-1])
      
      
      temp = as.zoo(historic_usage$hourly_load, order.by = historic_usage$hourly_load)
      
      FDC = data.frame(hourly_load_sorted = sort(historic_usage$hourly_load) ,
                       Percent = (fdc(temp, plot = FALSE)*100))
      
      
      plot_ly(FDC, x =~Percent, y =~hourly_load_sorted, type = "scatter", 
              mode = "lines", line = list(width = 6),
              hoverinfo = "text", 
              hovertext =~ paste("Load:", round(hourly_load_sorted,2), "MW",
                                 "<br>Duration:", round(Percent,2), "%"))%>%
        layout(xaxis = list(title = "<b>Duration %</b>"), 
               yaxis = list(title = "<b>Load [MW]</b>"))%>%
        config(displaylogo = FALSE,
               toImageButtonOptions = list(
                 filename = paste0("Historic Load Duration Curve",
                                   min(isolate(input$date_range_historic_data)), "-", 
                                   max(isolate(input$date_range_historic_data))),
                 format = "png",
                 width = 900,
                 height = 600
               ))
      
    }
    
    
  })
  
  
  
  #### Historic Download Handler ####
  output$download_Historic_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("NPCC Historic Data ",
                                 as.character(min(input$date_range_historic_data)), "-", as.character(max(input$date_range_historic_data)),
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      historic_data <- RV$historic_data_tab_data[,c(1,2)]
      
      Days = unique(substr(as.character(historic_data$Time), 1, 10))
      
      historic_data$Days = rep(Days, each = 24)
      
      historic_data$Hour = 1:24
      
      
      Formatted_historic_data = data.frame( matrix(nrow = (length(Days)), ncol = 25 ) )
      
      Formatted_historic_data$X1 = Days
      
      
      # hour_index = 1
      # day_index = 1
      
      for(hour_index in 1:24){
        
        for (day_index in 1:length(Days)) {
          
          Formatted_historic_data[Formatted_historic_data$X1 == Days[day_index],(hour_index+1)] = as.numeric(historic_data[historic_data$Days == Days[day_index] & 
                                                                                                                             historic_data$Hour == hour_index, 2])
        }
      }
      
      colnames(Formatted_historic_data) = c("Day / Hour", paste0("0",1:9,"00"), paste0(10:24,"00"))
      
      log_entry(req(RV$Username), "Downloaded Historic Data")
      
      write.csv(Formatted_historic_data, file, row.names = FALSE)
      
      
    })
  
  
  
  
  
  ###################### DISCOs #############################
  
  ##### Data Start - End Date #####
  output$DISCO_date_range_historic_data_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("DISCO_date_range_historic_data", "Select Date Range", 
                   start = Sys.Date()-14,
                   end = Sys.Date()-1,
                   min = "2016-07-01",
                   max = Sys.Date())
    
  })
  
  
  ##### Get and Filter Historic Data ####
  observeEvent(input$DISCO_historic_data_plot_button,{
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Getting Historic DISCO Data ... Please Wait")
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    DISCO_date_range_historic_data <- req(input$DISCO_date_range_historic_data)
    
    
    Historic_DISCO_Load <- RV$Historic_DISCO_Load
    
    Historic_DISCO_Load = Historic_DISCO_Load[, c(1, grep("Drawn", colnames(Historic_DISCO_Load)))]
    
    
    Historic_DISCO_Load_OLD = download_files_from_google_drive("Historic_DISCO_Load_OLD.csv")
    
    Historic_DISCO_Load_OLD = Historic_DISCO_Load_OLD[, c(1, grep("Drawn", colnames(Historic_DISCO_Load_OLD)))]
    
    
    ## Row bind both Old historic data and historic data
    DISCO_historic_tab_data = rbind(Historic_DISCO_Load_OLD, 
                                    Historic_DISCO_Load)
    
    ## Add Historic data to historic_data_plot_reactive() after filtering
    DISCO_historic_tab_data = DISCO_historic_tab_data[DISCO_historic_tab_data$Time >= as.POSIXct(paste0(min(DISCO_date_range_historic_data)," 00:00:00 PKT")) & 
                                                        DISCO_historic_tab_data$Time <= as.POSIXct(paste0(max(DISCO_date_range_historic_data)," 23:00:00 PKT")),]
    
    colnames(DISCO_historic_tab_data) = gsub("Drawn_", "",  colnames(DISCO_historic_tab_data))
    
    
    RV$DISCO_historic_tab_data = DISCO_historic_tab_data
    
    # remove it when done
    remove_modal_gif()
    
    ## Show the download historic data button
    # shinyjs::show("download_Historic_Data_div")
    
  })
  
  
  ##### Historic Stats Calculation ####
  output$DISCO_historic_data_stats_table <- renderDataTable({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait until some data is recieved in historic_data_plot_reactive()
    df_stats_metrics <- req(RV$DISCO_historic_tab_data)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    ## Make sure there are no NAs
    df_stats_metrics = df_stats_metrics[complete.cases(df_stats_metrics), c("Time", DISCOs_names)]
    
    temp = data.frame(
      Energy_GWh = round(colSums(df_stats_metrics[,-1])/1000,2),
      Peak_Power_MW = round(sapply(df_stats_metrics[,-1], max, na.rm = TRUE),2),
      Min_Power_MW = round(sapply(df_stats_metrics[,-1], min, na.rm = TRUE),2),
      Load_factor = sapply(df_stats_metrics[,-1], function(x){ round((sum(x, na.rm = TRUE) / (max(x, na.rm = TRUE) * nrow(df_stats_metrics))),2)})
    )
    
    
    
    colnames(temp) = c("Total Energy (GWh)", "Peak Power (MW)", "Min Power (MW)", "Load Factor")
    
    ## Display the newly uploaded data
    datatable({
      
      temp
      
    }, options = list(scrollX = TRUE, stateSave = TRUE, 
                      pageLength = 100, lengthChange = FALSE, 
                      dom = 't'), rownames= TRUE)
    
  })
  
  
  output$DISCO_barplot = renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait until some data is recieved in historic_data_plot_reactive()
    df_stats_metrics <- req(RV$DISCO_historic_tab_data)
    
    ## Make sure there are no NAs
    df_stats_metrics <- df_stats_metrics[complete.cases(df_stats_metrics),]
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    df_stats_metrics$Date = as.Date(df_stats_metrics$Time, tz = "Asia/Karachi")
    
    temp = cbind(data.frame(Time = unique(df_stats_metrics$Date)), 
                 data.frame(sapply(df_stats_metrics[, DISCOs_names], 
                                   function(x){ aggregate(x, list(df_stats_metrics$Date), 
                                                          sum, na.rm = TRUE)$x / 1000 })))
    
    
    p = plot_ly(temp, x = ~Time)
    
    for(i in 1:length(DISCOs_names)){
      
      
      p = p %>% add_trace(y = temp[[DISCOs_names[i]]], name = DISCOs_names[i], type = 'bar')
      
    }
    
    p %>%  layout(yaxis = list(title = "Energy (GWh)"),
                  xaxis = list(title = "Date"), barmode = 'stack')
    
  })
  
  
  
  #### Historic Download Handler ####
  output$DISCO_download_Historic_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("DISCOs Historic Data ",
                                 as.character(min(input$DISCO_date_range_historic_data)), "-", as.character(max(input$DISCO_date_range_historic_data)),
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      ## Wait until some data is recieved in historic_data_plot_reactive()
      DISCO_historic_tab_data <- req(RV$DISCO_historic_tab_data)
      
      log_entry(req(RV$Username), "Downloaded DISCO Historic Data")
      
      write.csv(DISCO_historic_tab_data, file, row.names = FALSE)
      
      
    })
  
  
  ## Select disco for historic data analysis
  output$DISCO_historic_tab_DISCO_selection_UI = renderUI({
    
    req(RV$DISCO_historic_tab_data)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    pickerInput(inputId = "DISCO_hidtoric_tab_DISCO_selection", 
                label = "Select DISCO",
                selected = DISCOs_names[1],
                choices = DISCOs_names,
                options = pickerOptions(dropupAuto = FALSE, `actions-box` = TRUE ),
                multiple = FALSE)
    
  })
  
  ## manage DISCO plot tabs
  output$Historic_DISCO_tabs = renderUI({
    
    if(req(RV$Logged_in_status)){
      
      DISCOs_names = req(RV$DISCOs_names)
      
      selected_DISCO = req(input$DISCO_hidtoric_tab_DISCO_selection)
      
      DISCO_historic_line_plot = req(RV$DISCO_historic_line_plot)
      DISCO_historic_pie_plot = req(RV$DISCO_historic_pie_plot)
      DISCO_historic_boxplot_plot = req(RV$DISCO_historic_boxplot_plot)
      DISCO_historic_heatmap = req(RV$DISCO_historic_heatmap)
      DISCO_historic_LDC = req(RV$DISCO_historic_LDC)
      
      # req(input$sidebar_tab_selected)
      
      Exclusions <- unlist(strsplit(
        as.character(RV$profiles$Exclusion[req(RV$profiles_index)]),";"))
      
      
      loading_screen("Please Wait")
      
      # if(any(Exclusions == "NPCC")){
      
      DISCO_tabs = do.call(tabsetPanel, c(lapply(1:length(selected_DISCO), function(i) {
        tabPanel(
          
          title = selected_DISCO, 
          
          br(),
          
          
          DISCO_historic_pie_plot[[i]],
          
          hr(),
          
          h3("Hourly Boxplot"),
          
          DISCO_historic_boxplot_plot[[i]],
          
          hr(),
          
          h3("Average Power Heatmap"),
          
          DISCO_historic_heatmap[[i]],
          
          hr(),
          
          h3("Load Duration Curve"),
          
          DISCO_historic_LDC[[i]],
          
          hr(),
          
          h3("Load Profile"),
          
          DISCO_historic_line_plot[[i]]
          
          
          
          
        )
      })))
      
      # }
      
      # remove it when done
      remove_modal_gif()
      
      DISCO_tabs
      
    }
    
  })
  
  ### DISCO Historic Individual Plots
  observe({
    
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    DISCO_historic_tab_data <- req(RV$DISCO_historic_tab_data)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    selected_DISCO <- req(input$DISCO_hidtoric_tab_DISCO_selection)
    
    DISCO_index <- which(DISCOs_names %in% selected_DISCO)
    
    DISCO_historic_line_plot = list()
    DISCO_historic_pie_plot = list()
    DISCO_historic_boxplot_plot = list()
    DISCO_historic_heatmap = list()
    DISCO_historic_LDC = list()
    
    for(i in DISCO_index){
      
      ## Line Plot
      {
        
        ## Line plot for hourly_load
        DISCO_historic_line_plot[[1]] = plot_ly()%>%
          add_trace(data = DISCO_historic_tab_data,
                    x=~Time, y = DISCO_historic_tab_data[[DISCOs_names[i]]], type = "scatter", mode = "lines", fill = "tozeroy",
                    hoverinfo = "text", 
                    hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                       "<br> Load :", round(DISCO_historic_tab_data[[DISCOs_names[i]]],2), "MW"))%>% 
          layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
                 yaxis = list( title = "Load [MW]"))%>%
          config(displaylogo = FALSE,
                 toImageButtonOptions = list(
                   filename = paste0(DISCOs_names[i], " Historic Line Plot ", min(isolate(input$DISCO_date_range_historic_data)), "-", 
                                     max(isolate(input$DISCO_date_range_historic_data))),
                   format = "png",
                   width = 900,
                   height = 600
                 ))
        
      }
      
      ## Pie Plot
      {
        ## Get the filtered data
        df_pie_plot <- DISCO_historic_tab_data[, c("Time", DISCOs_names[i])]
        colnames(df_pie_plot)[2] = "hourly_load"
        
        ## Make sure there are no NAs
        df_pie_plot = df_pie_plot[complete.cases(df_pie_plot$hourly_load),]
        
        ## Extract the weekdays from the time column
        df_pie_plot$Weekday = weekdays(df_pie_plot$Time)
        
        ## Calculate average for each weekday
        df_pie_plot = aggregate(df_pie_plot$hourly_load, list(df_pie_plot$Weekday), sum)
        
        ## Change column names
        colnames(df_pie_plot) = c("Day", "Energy")
        
        Days_vector = weekdays(as.Date("2020-07-05")+(1:7))
        
        df_pie_plot = df_pie_plot[match(Days_vector,df_pie_plot$Day),]
        
        ## Pie chart for weekdays average load
        DISCO_historic_pie_plot[[1]] = plot_ly(df_pie_plot, labels = ~Day, values = ~Energy, 
                                               type = 'pie',textinfo = 'percent', 
                                               hoverinfo = 'text',
                                               sort = FALSE,
                                               direction = "clockwise",
                                               text = ~paste(Day,":",round(Energy/1000,2),"GWh"))%>%
          layout(title = list(text =  paste0(DISCOs_names[i], " Energy Distribution Among Weekdays"), y = 1, xref = "paper", x = 2),
                 legend=list(title=list(text=paste("<b>Total Energy:", 
                                                   round(sum(df_pie_plot$Energy)/1000,2), "GWh<b>"))))%>%
          config(displaylogo = FALSE,
                 toImageButtonOptions = list(
                   filename = paste0(DISCOs_names[i], " Historic Pie Chart ", 
                                     min(isolate(input$DISCO_date_range_historic_data)), "-", 
                                     max(isolate(input$DISCO_date_range_historic_data))),
                   format = "png",
                   width = 900,
                   height = 600
                 ))
        
      }
      
      ## Hourly Bxplot
      {
        
        
        df_hourly_plot <- DISCO_historic_tab_data[, c("Time", DISCOs_names[i])]
        colnames(df_hourly_plot)[2] = "hourly_load"
        
        ## Make sure there are no NAs
        df_hourly_plot = df_hourly_plot[complete.cases(df_hourly_plot$hourly_load),c("Time", "hourly_load")]
        
        ## Get the hour for each row
        df_hourly_plot$Hour = sub("^0+", "", format(df_hourly_plot$Time, "%I %p"))
        
        df_hourly_plot$Hour = factor( df_hourly_plot$Hour, levels = df_hourly_plot$Hour[1:24])
        
        
        DISCO_historic_boxplot_plot[[1]] = plot_ly(df_hourly_plot, y = ~hourly_load, 
                                                   x = ~Hour, type = "box", hoverinfo = 'y')%>%
          layout(yaxis = list(title = "Load [MW]"))%>%
          config(displaylogo = FALSE,
                 toImageButtonOptions = list(
                   filename = paste0(DISCOs_names[i], " Historic Boxplot ", 
                                     min(isolate(input$DISCO_date_range_historic_data)), "-", 
                                     max(isolate(input$DISCO_date_range_historic_data))),
                   format = "png",
                   width = 900,
                   height = 600
                 ))
        
      }
      
      ## Heatmap
      {
        
        historic_data_tab_data <- DISCO_historic_tab_data[, c("Time", DISCOs_names[i])]
        colnames(historic_data_tab_data)[2] = "hourly_load"
        
        ## Check if data has data
        if (length(nrow(historic_data_tab_data)) != 0) {
          
          ## Make sure there are no NAs
          historic_usage = historic_data_tab_data[complete.cases(historic_data_tab_data$hourly_load),c("Time", "hourly_load")]
          
          ## Convert all columns to positive
          historic_usage[,-1] = abs(historic_usage[,-1])
          
          ## Get the hour for each row
          historic_usage$Hour = as.factor((as.POSIXlt(historic_usage$Time)$h)+1)
          
          ## Add day column
          historic_usage$Day = as.factor(weekdays(historic_usage$Time))
          
          ## only keep required columns
          historic_usage = historic_usage[,c("Hour", "Day", "hourly_load")]
          
          ## Get mean for each day of the week
          df_plot <- reshape2::dcast(data = historic_usage, Day~Hour, 
                                     fun.aggregate = mean,
                                     fill = 0,
                                     value.var = colnames(historic_usage)[3])
          
          ## Make weekdays, rowname of the dataframe
          row.names(df_plot) = df_plot$Day
          
          ## This variable helps in ordering the rows
          days.of.week <- as.factor(c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                      "Friday", "Saturday", "Sunday"))
          
          ## Ordering rows in dataframe
          df_plot = df_plot[order(df_plot$Day)[days.of.week], -1]
          
          ## Text to show on hovering
          Hover_text = matrix(nrow = nrow(df_plot), ncol = ncol(df_plot))
          
          x = seq.POSIXt(from = as.POSIXct("2020-09-07"), 
                         to = as.POSIXct("2020-09-07 23:00:00"), 
                         by = "hour")
          
          x = sub("^0+", "", format(x, "%I %p"))
          
          for(Day in 1:nrow(Hover_text)){
            
            for (Hour in 1:ncol(Hover_text)) {
              
              Hover_text[Day,Hour] = paste0("Day: ", row.names(df_plot)[Day], "\n",
                                            "Hour: ", x[Hour], "\n",
                                            "Power: ", round(df_plot[Day, Hour], 2), " MW")
              
            }
            
          }
          
          DISCO_historic_heatmap[[1]] = plot_ly(z = as.matrix(df_plot), y = rownames(df_plot), x = x, 
                                                colors = colorRamp(c("white", "blue")), type = "heatmap",
                                                hoverinfo='text',
                                                text= Hover_text,
                                                colorbar = list(title = "<b>Power [MW]</b>"))%>%
            layout(xaxis = list(title = "<b>Hour</b>"), 
                   yaxis = list(title = "<b>Day</b>"))%>%
            config(displaylogo = FALSE,
                   toImageButtonOptions = list(
                     filename = paste0(DISCOs_names[i], " Historic Heat Map ", 
                                       min(isolate(input$date_range_historic_data)), "-", 
                                       max(isolate(input$date_range_historic_data))),
                     format = "png",
                     width = 900,
                     height = 600
                   ))
          
        }
        
        
      }
      
      ## Load Duration Curve
      {
        
        historic_data_tab_data <- DISCO_historic_tab_data[, c("Time", DISCOs_names[i])]
        colnames(historic_data_tab_data)[2] = "hourly_load"
        
        ## Check if data has data
        if (length(nrow(historic_data_tab_data)) != 0) {
          
          ## Make sure there are no NAs
          historic_usage <- historic_data_tab_data[complete.cases(historic_data_tab_data$hourly_load),c("Time", "hourly_load")]
          
          ## Convert all columns to positive
          historic_usage[,-1] = abs(historic_usage[,-1])
          
          
          temp = as.zoo(historic_usage$hourly_load, order.by = historic_usage$hourly_load)
          
          FDC = data.frame(hourly_load_sorted = sort(historic_usage$hourly_load) ,
                           Percent = (fdc(temp, plot = FALSE)*100))
          
          
          DISCO_historic_LDC[[1]] = plot_ly(FDC, x =~Percent, y =~hourly_load_sorted, type = "scatter", 
                                            mode = "lines", line = list(width = 6),
                                            hoverinfo = "text", 
                                            hovertext =~ paste("Load:", round(hourly_load_sorted,2), "MW",
                                                               "<br>Duration:", round(Percent,2), "%"))%>%
            layout(xaxis = list(title = "<b>Duration %</b>"), 
                   yaxis = list(title = "<b>Load [MW]</b>"))%>%
            config(displaylogo = FALSE,
                   toImageButtonOptions = list(
                     filename = paste0(DISCOs_names[i], " Historic Load Duration Curve",
                                       min(isolate(input$date_range_historic_data)), "-", 
                                       max(isolate(input$date_range_historic_data))),
                     format = "png",
                     width = 900,
                     height = 600
                   ))
          
        }
        
        
      }
      
      
    }
    
    RV$DISCO_historic_line_plot = DISCO_historic_line_plot
    RV$DISCO_historic_pie_plot = DISCO_historic_pie_plot  
    RV$DISCO_historic_boxplot_plot = DISCO_historic_boxplot_plot
    RV$DISCO_historic_heatmap = DISCO_historic_heatmap
    RV$DISCO_historic_LDC = DISCO_historic_LDC
    
  })
  
  
  
  
  ##################### Model Performance ###################
  
  ##### Data Start - End Date #####
  output$date_range_model_performance_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("date_range_model_performance", "Select Date Range", 
                   start = Sys.Date()-15,
                   end = Sys.Date()-1,
                   min = "2016-07-01",
                   max = Sys.Date())
    
  })
  
  
  ##### Get and Filter Performance Data ####
  observeEvent(input$model_performance_plot_button,{
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Getting Performance Data ... Please Wait")
    
    Historic_NPCC_Load_Forecast_Weather = req(RV$Historic_NPCC_Load_Forecast_Weather)
    
    Historic_NPCC_Load_Forecast_Weather_OLD = 
      download_files_from_google_drive("Historic_NPCC_Load_Forecast_Weather_OLD.csv")
    
    ## Convert Time to posixct
    Historic_NPCC_Load_Forecast_Weather_OLD$Time = as.POSIXct(Historic_NPCC_Load_Forecast_Weather_OLD$Time)
    
    ## Row bind both Old historic data and historic data
    model_performance_tab_data = rbind(Historic_NPCC_Load_Forecast_Weather_OLD, 
                                       Historic_NPCC_Load_Forecast_Weather)
    
    ## Add Historic data to historic_data_plot_reactive() after filtering
    RV$model_performance_tab_data = model_performance_tab_data[model_performance_tab_data$Time >= as.POSIXct(paste0(min(input$date_range_model_performance)," 00:00:00 PKT")) & 
                                                                 model_performance_tab_data$Time <= as.POSIXct(paste0(max(input$date_range_model_performance)," 23:00:00 PKT")),]
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  
  ##### Error Distribution Plot ######
  output$model_performance_error_distribution_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait for model_performance_data_plot_reactive() to get data
    model_performance_tab_data = req(RV$model_performance_tab_data)
    
    ## Retrieve historic data
    df_error_dist_plot <- model_performance_tab_data
    
    ## Actual and predicted distributions
    p1 = plot_ly(alpha = 0.6) %>% 
      add_histogram(x = df_error_dist_plot$hourly_load, name = "Actual") %>% 
      add_histogram(x = df_error_dist_plot$Predicted, name = "Forecasted") %>% 
      layout(barmode = "overlay",
             xaxis = list( title = "Load"), 
             yaxis = list( title = "Frequency"))
    
    ## Error distributions
    p2 = plot_ly(x = df_error_dist_plot$hourly_load - df_error_dist_plot$Predicted, 
                 type = "histogram", name = "Error [Actual - Forecasted]")%>% layout(bargap=0.01,
                                                                                     xaxis = list( title = "Error [Actual - Forecasted]"), 
                                                                                     yaxis = list( title = "Frequency"))
    
    ## Plot actual, predicted and error distributions on same graph
    subplot(p1,p2,nrows = 2)
  })
  
  
  ##### Error Metrics Display ####
  output$model_performance_error_metrics <- renderUI({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait for model_performance_data_plot_reactive() to get data
    df_error_metrics = req(RV$model_performance_tab_data)
    
    Abs_Error = abs(df_error_metrics$hourly_load - 
                      df_error_metrics$Predicted)
    
    Max_abs_deviation = round(max(Abs_Error, na.rm = TRUE),2)
    
    Max_APE = round(max(100 * Abs_Error / df_error_metrics$hourly_load, na.rm = TRUE), 2)
    
    
    ## Calculate error metrics
    temp = data.frame(round(accuracy(df_error_metrics$hourly_load, df_error_metrics$Predicted),2))
    
    ## Display error metrics
    HTML(paste("<p style='font-size:20px;'>ME:",temp$ME," MW<br>RMSE:",temp$RMSE,
               " MW<br>MAE:",temp$MAE," MW<br>MPE:",temp$MPE,"%<br>MAPE:",temp$MAPE,
               "%<br>Max APE:",Max_APE,"%<br>Max Abs Deviation:",Max_abs_deviation," MW</p>"))
    
  })
  
  
  ##### Forecast vs Actual Plot ####
  output$model_performance_forecast_and_actual_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    ## Wait for model_performance_data_plot_reactive() to get data
    model_performance_tab_data <- req(RV$model_performance_tab_data)
    
    ## Plot Actual and predicted data
    plot_ly()%>%
      add_trace(data =model_performance_tab_data,
                x=~Time, y=~hourly_load, name = "Actual", type = "scatter", mode = "lines",
                hoverinfo = "text", 
                hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                   "<br> Actual Load :", round(hourly_load,2), "MW"))%>%
      add_trace(data = model_performance_tab_data,
                x=~Time, y=~Predicted, name = "Forecast", type = "scatter", mode = "lines",
                hoverinfo = "text", 
                hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                   "<br> Forecasted Load :", round(Predicted,2), "MW"))%>%
      layout(xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
             yaxis = list( title = "Load [MW]"))
    
  })
  
  
  #### Download model performance data ####
  output$download_model_performance_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("NPCC Model Performance Data ",
                                 as.character(min(input$date_range_model_performance)), 
                                 "-", as.character(max(input$date_range_model_performance)),
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      Model_performace <- RV$model_performance_tab_data[,c(1:3)]
      
      Days = unique(substr(as.character(Model_performace$Time), 1, 10))
      
      Model_performace$Absolute_Difference = abs(Model_performace$hourly_load - Model_performace$Predicted)
      
      Model_performace$Error_Percentage = 100*(abs(Model_performace$hourly_load - Model_performace$Predicted) / Model_performace$hourly_load)
      
      Model_performace$Days = rep(Days, each = 24)
      
      Model_performace$Hour = 1:24
      
      MAPE = NULL
      
      Max_abs_deviation = NULL
      
      Max_APE = NULL
      
      for(day_index in 1:length(Days)){
        
        temp = Model_performace[Model_performace$Days == Days[day_index],]
        
        if(!is.na(temp$Predicted[1])){
          
          MAPE = c(MAPE, rep(NA,3), accuracy(temp$hourly_load,temp$Predicted)[5])
          
          Abs_Error = abs(temp$hourly_load - temp$Predicted)
          
          Max_abs_deviation = c(Max_abs_deviation, rep(NA,3),round(max(Abs_Error, na.rm = TRUE),2))
          
          Max_APE = c(Max_APE, rep(NA,3), round(max(100 * Abs_Error / temp$hourly_load, na.rm = TRUE), 2))
          
        } else {
          
          MAPE = c(MAPE, rep(NA,4))
          Max_abs_deviation = c(Max_abs_deviation, rep(NA,4))
          Max_APE = c(Max_APE, rep(NA,4))
        }
        
        
      }
      
      
      Evaluation_form = data.frame( matrix(nrow = (length(Days)*4), ncol = 29 ) )
      
      Evaluation_form$X1 = rep(Days, each = 4)
      
      Evaluation_form$X2 = rep(c("Actual", "Predicted", "Difference", "Absolute_Percentage_Error"), (nrow(Evaluation_form)/4))
      
      
      
      # hour_index = 1
      # day_index = 1
      
      for(hour_index in 1:24){
        
        for (day_index in 1:length(Days)) {
          
          Evaluation_form[Evaluation_form$X1 == Days[day_index],(hour_index+2)] = as.numeric(Model_performace[Model_performace$Days == Days[day_index] & 
                                                                                                                Model_performace$Hour == hour_index, 2:5])
        }
      }
      
      Evaluation_form$X27 = MAPE
      Evaluation_form$X28 = Max_APE
      Evaluation_form$X29 = Max_abs_deviation
      
      colnames(Evaluation_form) = c("Day", "Hour", paste0("0",1:9,"00"), paste0(10:24,"00"), "MAPE", "MAX_APE", "Max_Absolute_Deviation"  )
      
      log_entry(req(RV$Username), "Downloaded Performance Data")
      
      write.csv(Evaluation_form, file, row.names = FALSE)
      
    })
  
  
  
  ################# DISCOs ############################
  
  ##### Data Start - End Date #####
  output$DISCOs_date_range_model_performance_UI <- renderUI({
    
    ## Start date for filtering historic data
    dateRangeInput("DISCOs_date_range_model_performance", "Select Date Range", 
                   start = Sys.Date()-15,
                   end = Sys.Date()-1,
                   min = "2016-07-01",
                   max = Sys.Date())
    
  })
  
  
  observeEvent(input$DISCOs_model_performance_plot_btn,{
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    loading_screen("Getting Performance Data ... Please Wait")
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    DISCOs_date_range_model_performance <- req(input$DISCOs_date_range_model_performance)
    
    PITC_Historic_Forecast <- RV$PITC_Historic_Forecast
    
    colnames(PITC_Historic_Forecast)[-1] = paste0(colnames(PITC_Historic_Forecast)[-1], "_Predicted")
    
    Historic_DISCO_Load <- RV$Historic_DISCO_Load
    
    Historic_DISCO_Load = Historic_DISCO_Load[, c(1, grep("Drawn", colnames(Historic_DISCO_Load)))]
    
    Predicted_historic_DISCO = merge(Historic_DISCO_Load, PITC_Historic_Forecast, all = TRUE)
    
    RV$Predicted_historic_DISCO <- Predicted_historic_DISCO[Predicted_historic_DISCO$Time >= as.POSIXct(paste0(min(DISCOs_date_range_model_performance)," 00:00:00 PKT")) &
                                                              Predicted_historic_DISCO$Time <= as.POSIXct(paste0(max(DISCOs_date_range_model_performance)," 23:00:00 PKT")),]
    
    # remove it when done
    remove_modal_gif()
    
  })
  
  
  ###### Error metrics table #####
  output$DISCO_error_metrics_table <- renderDataTable({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    Predicted_historic_DISCO <- req(RV$Predicted_historic_DISCO)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    Error_metrics = NULL
    
    for(i in 1:length(DISCOs_names)){
      
      temp = Predicted_historic_DISCO[, grep(DISCOs_names[i], colnames(Predicted_historic_DISCO))]
      
      Error_metrics = rbind(Error_metrics, as.data.frame(accuracy(temp[,1], temp[,2])))
      
    }
    
    # Error_metrics = cbind(data.frame(DISCOs = DISCOs_names), Error_metrics)
    
    rownames(Error_metrics) = DISCOs_names
    
    Error_metrics = round_df(Error_metrics, 2)
    
    ## Display the newly uploaded data
    datatable({
      
      Error_metrics
      
    }, options = list(scrollX = TRUE, stateSave = TRUE, 
                      pageLength = 100, lengthChange = FALSE, 
                      dom = 't'), rownames= TRUE)
    
  })
  
  
  ##### Error Distribution Plot ######
  output$DISCO_model_performance_error_distribution_plot <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    Predicted_historic_DISCO <- req(RV$Predicted_historic_DISCO)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    Error_df = data.frame(matrix(nrow = nrow(Predicted_historic_DISCO), ncol = length(DISCOs_names)))
    
    colnames(Error_df) = DISCOs_names
    
    # i = 1
    for(i in 1:length(DISCOs_names)){
      
      temp = Predicted_historic_DISCO[, grep(DISCOs_names[i], colnames(Predicted_historic_DISCO))]
      
      Error_df[[DISCOs_names[i]]] = temp[,1] - temp[,2]
      
    }
    
    
    fig <- plot_ly(type = "box")
    
    for(i in 1:length(DISCOs_names)){
      
      fig <- fig %>% add_trace(y = Error_df[[DISCOs_names[i]]], quartilemethod="inclusive", name = DISCOs_names[i])
      
    }
    
    fig %>%
      layout(xaxis = list( title = "DISCOs" ), 
             yaxis = list( title = "Error (Actual - Predicted) [MW]"))
    
  })
  
  
  ##### Forecast vs Actual Plot ####
  output$DISCO_model_performance_forecast_and_actual_plotly <- renderPlotly({
    
    ## Requires that the user is logged in
    req(RV$Logged_in_status)
    
    Predicted_historic_DISCO <- req(RV$Predicted_historic_DISCO)
    
    DISCOs_names <- req(RV$DISCOs_names)
    
    forecast_plot = list()
    
    # i = 1
    for(i in 1:length(DISCOs_names)){
      
      temp = Predicted_historic_DISCO[, c(1,grep(DISCOs_names[i], 
                                                 colnames(Predicted_historic_DISCO)))]
      
      ## Plot Actual and predicted data
      forecast_plot[[i]] = plot_ly(showlegend = FALSE)%>%
        add_trace(data =temp,
                  x=~Time, y = temp[,2], name = "Actual", type = "scatter", mode = "lines",
                  hoverinfo = "text", 
                  hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                     "<br> Actual Load :", round(temp[,2],2), "MW"))%>%
        add_trace(data = temp,
                  x=~Time, y = temp[,3], name = "Forecast", type = "scatter", mode = "lines",
                  hoverinfo = "text", 
                  hovertext =~ paste("Time :", format(Time, format = "%d-%b-%Y %I %p"),
                                     "<br> Forecasted Load :", round(temp[,3],2), "MW"))%>%
        layout(
          xaxis = list( title = "Time", tickformat = "%I %p<br>%d %B (%a)<br>%Y" ), 
          yaxis = list( title = paste0(DISCOs_names[i],"")))
      
      
    }
    
    subplot(forecast_plot, nrows = 5, shareX = TRUE, titleY = TRUE, margin = 0.03)
    
    
    
  })
  
  
  #### Download model performance data ####
  output$DISCO_download_model_performance_Data <- downloadHandler(
    
    ## Generates name of the file to be downloaded
    filename = function() {paste("DISCOs Model Performance Data ",
                                 as.character(min(input$DISCOs_date_range_model_performance)), 
                                 "-", as.character(max(input$DISCOs_date_range_model_performance)),
                                 ".csv", sep = "")},
    
    ## Actual function to invoke downloading of file by the internet browser
    content = function(file) {
      
      ## Requires that the user is logged in
      req(RV$Logged_in_status)
      
      Predicted_historic_DISCO <- req(RV$Predicted_historic_DISCO)
      
      DISCOs_names <- req(RV$DISCOs_names)
      
      colnames(Predicted_historic_DISCO)[grep("Drawn", colnames(Predicted_historic_DISCO))] = 
        gsub("Drawn","Actual",colnames(Predicted_historic_DISCO)[grep("Drawn", colnames(Predicted_historic_DISCO))]) 
      
      log_entry(req(RV$Username), "Downloaded DISCOs Performance Data")
      
      write.csv(Predicted_historic_DISCO, file, row.names = FALSE)
      
    })
  
  
}



shinyApp(ui, server)



