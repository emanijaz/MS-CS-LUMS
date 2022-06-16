library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(Metrics)
library(caTools)
library(MASS)
library(ggplot2)
library(googledrive)
options(shiny.maxRequestSize=30*1024^2)
library(tibble)
library(sqldf)
library(caret)

# memory.size(max = FALSE)
# memory.limit(size = NA)
# Sys.setenv('R_MAX_VSIZE'=32000000000)
set.seed(2)
# Sys.setenv(TZ="Asia/Karachi")

options(gargle_verbosity = "debug")
# designate project-specific cache
options(gargle_oauth_cache = ".secrets", gargle_oauth_email = 'emangpu583@gmail.com')
# check the value of the option, if you like
gargle::gargle_oauth_cache()
# trigger auth on purpose --> store a token in the specified cache
drive_auth()

# see your token file in the cache, if you like
list.files(".secrets/")

filename = '2016-2020-Historic-Data.csv'
shinyServer (function(input, output, session){
  
  getcolumns <- function(){
    column_names <- c('Time','hourly_load',	'Predicted',	'summary',	'icon',	'precipIntensity',	'precipProbability',	'temperature'	,'apparentTemperature',	'dewPoint',	'humidity',	'pressure',	'windSpeed',	'windGust')
    cities <- c('Lahore', 'Multan', 'Faislabad', 'Quetta', 'Peshawar', 'Gujranwala', 'Hyderabad', 'Sukkur', 'Islamabad')
    
    actual_cols <- c()
    actual_cols<-append(actual_cols, column_names[0])
    actual_cols<-append(actual_cols, column_names[1])
    actual_cols<-append(actual_cols, column_names[2])
    actual_cols<-append(actual_cols, column_names[3])
    for(city in cities){
      city_name = paste0(city,'_')
      for(col in column_names[c(-1,-2,-3)]){
        actual_cols<-append(actual_cols, paste0(city_name, col))
      }
    }
    actual_cols
  }
  actual_cols = getcolumns()
 
  preprocess_data<- function(new_data, prev_data){
    
    ## ASSUME : NO MISSING DATA IS PROVIDED i-e, no missing hrs 
    
    
    if(length(names(new_data)) != length(actual_cols)){
      
      missing_cols = setdiff(actual_cols, names(new_data))
      print( missing_cols)
      # new_data[missing_cols]<-0 ## filling missing cols with 0
    }
    
    new_start_date = new_data$Time[1]
    new_end_date = tail(new_data$Time, n=1)
    prev_start_date = prev_data$Time[1];
    prev_end_date = tail(prev_data$Time, n=1)
    
    if((new_start_date <= prev_end_date) & (new_end_date <= prev_end_date)){  ## data overwrite, already existing
      to_update = prev_data[prev_data$Time %in% new_data$Time,]
      to_update[-1][!is.na(new_data[-1])] = new_data[-1][!is.na(new_data[-1])]
      existing_data = prev_data[!prev_data$Time %in% new_data$Time,]
      to_update = rbind(to_update, existing_data)
      new_data = to_update[order(to_update$Time),]
    }
    else if((new_start_date <= prev_end_date) && (new_end_date > prev_end_date)){  ## some new data and some existing
   
      new_data_existing = subset(new_data, new_data$Time >= new_start_date & new_data$Time <=prev_end_date)
      
      to_update = prev_data[prev_data$Time %in% new_data_existing$Time,]
      to_update[-1][!is.na(new_data_existing[-1])] = new_data_existing[-1][!is.na(new_data_existing[-1])]
      existing_data = prev_data[!prev_data$Time %in% new_data_existing$Time,]
      to_update = rbind(to_update, existing_data)
      past_data = to_update[order(to_update$Time),]
      
      new_data = subset(new_data, new_data$Time > prev_end_date & new_data$Time <= new_end_date)
      new_data = rbind(past_data, new_data)
      
    }
    else{ ## data belongs to future  -- new_start_date > prev_end_date
      new_data = rbind(prev_data, new_data)
    }
    
    new_data[ , c(-1)] <- as.data.frame(apply(new_data[ , c(-1)], 2, as.numeric))
    return(new_data)
  }
  
  NewData <- reactive({
    file_to_read = input$file
    
    if(is.null(file_to_read)){ # get previously uploaded data from drive and display
      # file.remove(filename) ## using for testing currently
      if(file.exists(filename)){   ## if locally exist then use that, no need to download from drive
        
        print('yes file exists locally!')
        data<- read.csv(filename , sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
        
      }
      else{                   ## get file from drive
        print('file dont exist locally!')
        print('downloading file from drive!')
        drive_download(filename, path = filename  , overwrite = TRUE)
        print('file downloaded from drive!')
        data = read.csv(filename)
        write.csv(data, filename, row.names = FALSE) ## writing data locally
       
      }
      
    }
    else{ ## get new data, update locally and on drive
      
      new_data<- read.csv(file_to_read$datapath, sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
      prev_data<- read.csv(filename , sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
      
      data = preprocess_data(new_data, prev_data)
      print('upating locally!')
      write.csv(data, filename, row.names=FALSE) ## updating locally 
      print('uploading data to drive!')
      drive_upload(filename, filename, overwrite = TRUE) ## updating drive data
      print('data uploaded to drive!')
  
    }
    
    # data<- read.csv(file_to_read$datapath, sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
    # updateSelectInput(session, "var1", choices = names(data), selected = names(data)[1]) ## writing it here but needs to modify
    # updateSelectInput(session, "var2", choices = names(data), selected = names(data)[2])## writing it here but needs to modify
    
    
    return(data)
  })
  
  
  
  output$output_table <- renderDT({
    
    tail(NewData(), 50)  ## show only last 50 rows
    
  })
  
  output$forecast_total_monthly <- renderPlotly({
    
    linear_model_monthly_energy_forecast(NewData())  ## returns forecast plot
    
  })
  
  output$forecast_monthly_peak <- renderPlotly({
   
    linear_model_monthly_peak_forecast(NewData())  ## returns forecast plot
    
  })
  
  
  ## MODELS DEFINED HERE
  
  ## 1. LINEAR MODEL
  
  linear_model_monthly_energy_forecast <- function(data){
    
    selected_cols = c('hourly_load','Faislabad_apparentTemperature','Peshawar_humidity','Hyderabad_humidity','Faislabad_windSpeed','Quetta_dewPoint','Gujranwala_windSpeed', 'month','year')
    data = data[(25561:39482),]  ## getting 2019  and 2020 data

    data = data_preprocessing(data)
 
    total_energy_df <- aggregate(. ~month+year, data, sum)
    total_energy_df = dplyr::select(total_energy_df, selected_cols)
    train_data = head(total_energy_df, 15)
    valid_data = tail(total_energy_df, 4)
    
    Model = lm(hourly_load~Faislabad_apparentTemperature+Peshawar_humidity+Hyderabad_humidity+Faislabad_windSpeed+Quetta_dewPoint+Gujranwala_windSpeed, data=train_data)
    ModelSummary = summary(Model)
    # print(ModelSummary)
    # valid_preds <- predict(Model, valid_data)
    # valid_actuals <- valid_data$hourly_load
    # printStats(ModelSummary, valid_actuals, valid_preds)
    
    filename_2021 = 'NPCC Historic Data 2021-01-01-2021-07-31.csv'
    data_2021 = read.csv(filename_2021 , sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
    data_2021 = data_preprocessing(data_2021)
    test_data_2021 <- aggregate(. ~month+year, data_2021, sum)
    test_data_2021 = dplyr::select(test_data_2021,selected_cols)
    pred_2021_load <- predict(Model, test_data_2021)
    actual_2021_load <- test_data_2021$hourly_load
    output$total_monthly_energy_metrics<-printStats(ModelSummary, actual_2021_load, pred_2021_load)
    
    ## printing of graph
  
    forecast_plot(total_energy_df, test_data_2021, pred_2021_load, "Monthly Energy Forecast")
  }
  
  linear_model_monthly_peak_forecast <- function(data){
    selected_cols = c('hourly_load','Faislabad_apparentTemperature','Peshawar_humidity','Hyderabad_humidity','Gujranwala_windSpeed', 'month','year')
    data = data[(25561:39482),]  ## getting 2019  and 2020 data
    data = data_preprocessing(data)
    total_peak_power = sqldf("SELECT *,max(hourly_load) as dup_hourly_load from data group by year,month")
    total_peak_power = total_peak_power[,-which(names(total_peak_power) == 'dup_hourly_load')]
    total_peak_power = dplyr::select(total_peak_power, selected_cols)
    train_data = head(total_peak_power, 15)
    valid_data = tail(total_peak_power, 4)
    
    Model = lm(hourly_load~Faislabad_apparentTemperature+Peshawar_humidity+Hyderabad_humidity+Gujranwala_windSpeed, data=train_data)
    ModelSummary = summary(Model)
    # print(ModelSummary)
    # valid_preds <- predict(Model, valid_data)
    # valid_actuals <- valid_data$hourly_load
    # printStats(ModelSummary, valid_actuals, valid_preds)

     filename_2021 = 'NPCC Historic Data 2021-01-01-2021-07-31.csv'
     data_2021 = read.csv(filename_2021 , sep=',', header=TRUE,fill = TRUE, stringsAsFactors = FALSE)
     data_2021 = data_preprocessing(data_2021)
    
     test_data_2021 = sqldf("SELECT *,max(hourly_load) as dup_hourly_load from data_2021 group by year,month")
     test_data_2021 = test_data_2021[,-which(names(test_data_2021) == 'dup_hourly_load')]
    
     test_data_2021 = dplyr::select(test_data_2021,selected_cols)
     pred_2021_load <- predict(Model, test_data_2021)
     actual_2021_load <- test_data_2021$hourly_load
     output$peak_power_energy_metrics<-printStats(ModelSummary, actual_2021_load, pred_2021_load)

     forecast_plot(total_peak_power, test_data_2021, pred_2021_load, "Peak Power Forecast")
  }
  
  data_preprocessing <- function(data){
    data = subset(data, !is.na(hourly_load)) ## 2019 DEC, last some days having NA for hourly_load, removing them
    data = data[,c(-4,-5,-15,-16,-26,-27,-37,-38,-48,-49,-59,-60,-70,-71,-81,-82,-92,-93)] ## removing categorical cols
    data$month <- strftime(data$Time, '%m')
    data$year <- strftime(data$Time, '%Y')
    data= data[,c(-1)] ## exclude time column
    data[is.na(data)] = 0  ## fill missing values with 0
    
    data
  }
  
  forecast_plot <- function(previous_years_data, test_data_2021, pred_2021_load, title_name){
    x_axis = c('June 2019','July 2019','Aug 2019','Sep 2019','Oct 2019','Nov 2019','Dec 2019','Jan 2020','Feb 2020','March 2020','April 2020','May 2020','June 2020','July 2020','Aug 2020','Sep 2020','Oct 2020','Nov 2020', 'Dec 2020',
               'Jan 2021', 'Feb 2021', 'March 2021', 'April 2021', 'May 2021', 'June 2021', 'July 2021')
    
    xform <- list(categoryorder = "array",
                  categoryarray = x_axis)
    
    actual_hourly_load = rbind(previous_years_data, test_data_2021)$hourly_load
    fig1<-plot_ly(rbind(previous_years_data, test_data_2021), x=x_axis, y= actual_hourly_load, mode='markers+lines', type='scatter', name="Actual")
    fig1 <- fig1 %>% add_trace(y = append(rep.int(NA,nrow(previous_years_data)), pred_2021_load), name = 'Forecast 2021', mode = 'markers+lines')
    fig1<- fig1 %>% layout(title =title_name,yaxis=list(title="Load MW"), xaxis=xform)
    fig1
  }
  
  printStats <- function(ModelSummary, actual, pred){
     renderUI({
      HTML(paste("<p style='font-size:15px;'><strong>R square:</strong>",RSQUARE(actual,pred),
                 "<br><strong>RMSE:</strong>",rmse(actual, pred),
                 "MW<br><strong>RMS:</strong>" , mean(ModelSummary$residuals^2),
                 " MW<br><strong>MAE:</strong>",mae(actual, pred),
                 "<br><strong>MASE:</strong>",mase(actual, pred),
                 "%<br><strong>MAPE:</strong>",mape(actual, pred),
                 "%<br><strong>RAE:</strong>",rae(actual, pred)))
    })
  }
  
  ## analytics tab
  
  output$time_vs_hourly_load_plot <- renderPlotly({
    data_time_vs_hourly_load = NewData()
    data_time_vs_hourly_load$Time <- as.POSIXct(data_time_vs_hourly_load$Time)
    yaxis <- list(title="Hourly Load")
    fig <- plot_ly(data_time_vs_hourly_load, x = data_time_vs_hourly_load$Time, y= data_time_vs_hourly_load$hourly_load, mode='lines', type='scatter')
    fig <- fig %>% layout(yaxis=yaxis, title=(list="Time and Hourly Load"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    fig
  })
  
  ## total monthly energy plots ###############
  output$total_monthly_energy_line_plot <- renderPlotly({
    data_total_monthly_energy = NewData()
    data_total_monthly_energy$Time <- as.POSIXct(data_total_monthly_energy$Time)
    data_total_monthly_energy$year = format(data_total_monthly_energy$Time, format="%Y")
    data_total_monthly_energy$month = months(as.Date(data_total_monthly_energy$Time))
    data_total_monthly_energy$month_num = format(data_total_monthly_energy$Time, format="%m")
    
    monthly_total_energy = sqldf("SELECT month_num, year, avg(hourly_load) as total_energy from data_total_monthly_energy group by month_num, year")
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   tickmode = "array", title="Months")
    
    yaxis <- list(title="Total Energy (MW)")
    fig <- plot_ly(monthly_total_energy, x=monthly_total_energy$month_num, y=monthly_total_energy$total_energy,
                    color = monthly_total_energy$year, type='scatter', mode='lines',colors = "Dark2")
    fig <- fig %>% layout(xaxis = x_axis, yaxis=yaxis, title=(list="Total Monthly Energy"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
 
  })
  
  output$total_monthly_energy_box_plot <- renderPlotly({
    data_total_monthly_energy = NewData()
    data_total_monthly_energy$Time <- as.POSIXct(data_total_monthly_energy$Time)
    data_total_monthly_energy$year = format(data_total_monthly_energy$Time, format="%Y")
    data_total_monthly_energy$month = months(as.Date(data_total_monthly_energy$Time))
    data_total_monthly_energy$month_num = format(data_total_monthly_energy$Time, format="%m")
    
    monthly_total_energy = sqldf("SELECT month_num, year, avg(hourly_load) as total_energy from data_total_monthly_energy group by month_num, year")
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                   tickmode = "array", title="Months")
    
    yaxis <- list(title="Total Energy (MW)")
    fig <- plot_ly(monthly_total_energy, x=monthly_total_energy$month_num, y=monthly_total_energy$total_energy,
                    color = monthly_total_energy$year, type='bar',colors = "Dark2")
    fig <- fig %>% layout(xaxis = x_axis, yaxis=yaxis, title=(list="Total Monthly Energy"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    
  })
  
  
  
  ## total peak power energy plots
  output$peak_power_energy_line_plot <- renderPlotly({
    data_total_peak_energy = NewData()
    data_total_peak_energy$Time <- as.POSIXct(data_total_peak_energy$Time)
    data_total_peak_energy$year = format(data_total_peak_energy$Time, format="%Y")
    data_total_peak_energy$month = months(as.Date(data_total_peak_energy$Time))
    data_total_peak_energy$month_num = format(data_total_peak_energy$Time, format="%m")
    
    monthly_peak_energy = sqldf("SELECT month_num, year, max(hourly_load) as peak_power from data_total_peak_energy group by month_num, year")
    
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),tickmode = "array", title="Months")
    
    yaxis <- list(title="Peak Power (MW)")
    fig <- plot_ly(monthly_peak_energy, x=monthly_peak_energy$month_num, y=monthly_peak_energy$peak_power,
                    color = monthly_peak_energy$year, type='scatter', mode='lines',colors = "Set1")
    fig <- fig %>% layout(xaxis = x_axis, yaxis=yaxis, title=(list="Monthly Peak Power"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    
  })
  
  output$peak_power_energy_energy_box_plot <- renderPlotly({
    data_total_peak_energy = NewData()
    data_total_peak_energy$Time <- as.POSIXct(data_total_peak_energy$Time)
    data_total_peak_energy$year = format(data_total_peak_energy$Time, format="%Y")
    data_total_peak_energy$month = months(as.Date(data_total_peak_energy$Time))
    data_total_peak_energy$month_num = format(data_total_peak_energy$Time, format="%m")
    
    monthly_peak_energy = sqldf("SELECT month_num, year, max(hourly_load) as peak_power from data_total_peak_energy group by month_num, year")
    
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),tickmode = "array", title="Months")
    
    yaxis <- list(title="Peak Power (MW)")
    fig <- plot_ly(monthly_peak_energy, x=monthly_peak_energy$month_num, y=monthly_peak_energy$peak_power,
                    color = monthly_peak_energy$year, type='bar',colors = "Set1")
    fig <- fig %>% layout(xaxis = x_axis, yaxis=yaxis, title=(list="Monthly Peak Power"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    
  })
  
  output$yearly_energy_box_plot1 <- renderPlotly({
    data_yearly_energy = NewData()
    data_yearly_energy$Time <- as.POSIXct(data_yearly_energy$Time)
    data_yearly_energy$year = format(data_yearly_energy$Time, format="%Y")
    data_yearly_energy$month = months(as.Date(data_yearly_energy$Time))
    data_yearly_energy$month_num = format(data_yearly_energy$Time, format="%m")
    
    box_plot_data_2016 = sqldf("SELECT month_num, year, hourly_load from data_yearly_energy where year = '2016' ")
    
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),tickmode = "array", title="Months")
    
    fig5 <- plot_ly(box_plot_data_2016, x=box_plot_data_2016$month_num, y=box_plot_data_2016$hourly_load,
                    type='box')
    fig5 <- fig5 %>% layout(xaxis = x_axis)
    
    box_plot_data_2017 = sqldf("SELECT month_num, year, hourly_load from data_yearly_energy where year = '2017' ")
    fig6 <- plot_ly(box_plot_data_2017, x=box_plot_data_2017$month_num, y=box_plot_data_2017$hourly_load,
                    type='box')
    fig6 <- fig6 %>% layout(xaxis = x_axis)
    
    box_plot_data_2018 = sqldf("SELECT month_num, year, hourly_load from data_yearly_energy where year = '2018' ")
    fig7 <- plot_ly(box_plot_data_2018, x=box_plot_data_2018$month_num, y=box_plot_data_2018$hourly_load,
                    type='box')
    fig7 <- fig7 %>% layout(xaxis = x_axis)
    
    box_plot_data_2019 = sqldf("SELECT month_num, year, hourly_load from data_yearly_energy where year = '2019' ")
    fig8 <- plot_ly(box_plot_data_2017, x=box_plot_data_2019$month_num, y=box_plot_data_2019$hourly_load,
                    type='box')
    fig8 <- fig8 %>% layout(xaxis = x_axis)
    
    box_plot_data_2020 = sqldf("SELECT month_num, year, hourly_load from data_yearly_energy where year = '2020' ")
    fig9 <- plot_ly(box_plot_data_2020, x=box_plot_data_2020$month_num, y=box_plot_data_2020$hourly_load,
                    type='box')
    fig9 <- fig9 %>% layout(xaxis = x_axis, paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    fig <- subplot(fig5, fig6, fig7, fig8, fig9, nrows = 3, margin = 0.08)
   
    fig
    
  })
  output$yearly_energy_box_plot2 <- renderPlotly({
    data_total_peak_energy = NewData()
    data_total_peak_energy$Time <- as.POSIXct(data_total_peak_energy$Time)
    data_total_peak_energy$year = format(data_total_peak_energy$Time, format="%Y")
    data_total_peak_energy$month = months(as.Date(data_total_peak_energy$Time))
    data_total_peak_energy$month_num = format(data_total_peak_energy$Time, format="%m")
    
    x_axis <- list(ticktext=list("Jan", "Feb", "March", "April", "May", "June", "July", "August", "Sep", "Oct", "Nov", "Dec" ), tickvals = list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),tickmode = "array", title="Months")
    myfig = plot_ly(data_total_peak_energy, x =data_total_peak_energy$month_num, y= data_total_peak_energy$hourly_load, color = data_total_peak_energy$year, type='box',colors = "Paired")
    myfig <- myfig %>% layout(boxmode = "group", xaxis = x_axis, yaxis = list(title='Hourly Load (MW)'), title=(list="Monthly Energy"), paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
    myfig
    
  })
  
  
  
})