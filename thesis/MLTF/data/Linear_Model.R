library(data.table)
library(forecast)
library(plotly)
library("plotly")

Historic_NPCC_2016_2020 = read.csv('2016-2020-Historic-Data.csv')[,-3] ## remove Predicted Column

str(Historic_NPCC_2016_2020)

Historic_NPCC_2016_2020$Time = as.POSIXct(Historic_NPCC_2016_2020$Time)

summary(Historic_NPCC_2016_2020)

class(Historic_NPCC_2016_2020)

tmp = as.character(unlist(sapply(Historic_NPCC_2016_2020, class)))[-2]  ## removing 2nd time column


#### correlation ####
{

correlation = data.frame(cor(Historic_NPCC_2016_2020[, which( tmp == "numeric" )], 
                             use = "complete.obs"))

class(correlation$hourly_load)

initially_selected_variables = row.names(correlation[correlation$hourly_load >= 0.7 |
                          correlation$hourly_load <= -0.7,])

multicolenarity_check = data.frame(cor(Historic_NPCC_2016_2020[,initially_selected_variables],
                                       use = "complete.obs"))

row.names(multicolenarity_check[multicolenarity_check$Lahore_apparentTemperature < 0.8 &
                         multicolenarity_check$Lahore_apparentTemperature > -0.8,])

}

## aggregation on basis of monthly energy and peak power
{
Filtered_Historic_NPCC_2016_2020 = Historic_NPCC_2016_2020[, c(1,2,8)] ## fetch Time, hourly_load, Lahore_Apparent_Temperature
 
Filtered_Historic_NPCC_2016_2020 = Filtered_Historic_NPCC_2016_2020[complete.cases(Filtered_Historic_NPCC_2016_2020), ]

# summary(Filtered_Historic_NPCC_2016_2020)


Filtered_Historic_NPCC_2016_2020$Month_Year = paste0(format(Filtered_Historic_NPCC_2016_2020$Time, format = "%B"), "_",  
                                                    format(Filtered_Historic_NPCC_2016_2020$Time, format = "%Y"))

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = aggregate(Filtered_Historic_NPCC_2016_2020$hourly_load, 
                                                                list(Filtered_Historic_NPCC_2016_2020$Month_Year), sum) 

colnames(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020) = c("Month_Year", "NPCC_Monthly_Energy_MWh")


tmp = aggregate(cbind(Filtered_Historic_NPCC_2016_2020$hourly_load,Filtered_Historic_NPCC_2016_2020$Lahore_apparentTemperature),
                list(Filtered_Historic_NPCC_2016_2020$Month_Year), max) 

colnames(tmp) = c("Month_Year", "NPCC_Monthly_Peak_Power_MW", "NPCC_Monthly_Max_Lahore_Apparent_Temperature_C")

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = merge(tmp, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020)

## Min apparent temperature

tmp = aggregate(Filtered_Historic_NPCC_2016_2020$Lahore_apparentTemperature,
                list(Filtered_Historic_NPCC_2016_2020$Month_Year), min) 

colnames(tmp) = c("Month_Year","NPCC_Monthly_Min_Lahore_Apparent_Temperature_C")

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = merge(tmp, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020)

## Mean apparent temperature
tmp = aggregate(Filtered_Historic_NPCC_2016_2020$Lahore_apparentTemperature,
                list(Filtered_Historic_NPCC_2016_2020$Month_Year), mean) 

colnames(tmp) = c("Month_Year","NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C")

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = merge(tmp, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020)

# Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,-c(3,5)] ## remove 

tmp = data.frame(Month_Year =paste0( month.name, "_2021")[1:10])

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = merge(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020, tmp, all = TRUE)



Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$Date = 
  as.Date(paste("1" , Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$Month_Year), format = "%d %B_%Y")

Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020 = 
  Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[order(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$Date), ]

}

## applying Linear Model

tmp = cor(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,-c(1,7)], use = "complete.obs") ## removing Month_Year and Date columns

tmp = acf(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C)



Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$Lag_11_NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C = 
  shift(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C, 11)


## finding correlation between Lahore Apparent temperature and Lagged Lahore Apparent Temperature 11 values
cor(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,c(2,6)], use = "complete.obs")


Apparent_Temperature_LM = lm(NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C~Lag_11_NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C,
                             data=Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020)


summary(Apparent_Temperature_LM)

row.names(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020) = 1:nrow(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020)

Predicted_Apparent_Temperature =  predict(Apparent_Temperature_LM, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[25:34,]) ## giving rows to be predicted  as input



## find error metrics for apparent temperature


## model for Energy and Peak Power using predicted temperature

Monthly_Energy_LM = lm(NPCC_Monthly_Energy_MWh~NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C,
                       data=Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[1:24,])


#plot(Monthly_Energy_LM )  ## add to thesis in detail

summary(Monthly_Energy_LM)


Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020$NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C[25:34] = Predicted_Apparent_Temperature


Predicted_Monthly_Energy = predict(Monthly_Energy_LM, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[25:34,])


Monthly_Energy_Metrics = 
  forecast::accuracy(Predicted_Monthly_Energy, Monthly_Aggregated_Filtered_Historic_NPCC_2021$NPCC_Monthly_Energy_MWh)


## find energy and peak power error metrics

Monthly_Peak_Power_LM = lm(NPCC_Monthly_Peak_Power_MW~NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C,
                           data=Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[1:24,])


summary(Monthly_Peak_Power_LM)

Predicted_Monthly_Peak_power = predict(Monthly_Peak_Power_LM, Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[25:34,])


Monthly_Peak_Power_Metrics =
  forecast::accuracy(Predicted_Monthly_Peak_power, Monthly_Aggregated_Filtered_Historic_NPCC_2021$NPCC_Monthly_Peak_Power_MW)







{

Historic_NPCC_2021 = read.csv("NPCC Historic Data 2021-01-01-2021-10-31.csv")[,-3]

Historic_NPCC_2021$Time = as.POSIXct(Historic_NPCC_2021$Time)

summary(Historic_NPCC_2021)

class(Historic_NPCC_2021)

Historic_NPCC_2021 = Historic_NPCC_2021[, c(1,2,8)] ## fetch Time,hourly_load, Lahore_Apparent_Temperature

Historic_NPCC_2021 = Historic_NPCC_2021[complete.cases(Historic_NPCC_2021), ]


Historic_NPCC_2021$Month_Year = paste0(format(Historic_NPCC_2021$Time, format = "%B"), "_",
                                                     format(Historic_NPCC_2021$Time, format = "%Y"))

Monthly_Aggregated_Filtered_Historic_NPCC_2021 = aggregate(Historic_NPCC_2021$hourly_load,
                                                                list(Historic_NPCC_2021$Month_Year), sum)

colnames(Monthly_Aggregated_Filtered_Historic_NPCC_2021) = c("Month_Year", "NPCC_Monthly_Energy_MWh")


tmp = aggregate(Historic_NPCC_2021$Lahore_apparentTemperature, list(Historic_NPCC_2021$Month_Year), mean)

colnames(tmp) = c("Month_Year", "NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C")

Monthly_Aggregated_Filtered_Historic_NPCC_2021 = merge(tmp, Monthly_Aggregated_Filtered_Historic_NPCC_2021)


tmp = aggregate(Historic_NPCC_2021$hourly_load,
                list(Historic_NPCC_2021$Month_Year), max)

colnames(tmp) = c("Month_Year", "NPCC_Monthly_Peak_Power_MW")

Monthly_Aggregated_Filtered_Historic_NPCC_2021 = merge(tmp, Monthly_Aggregated_Filtered_Historic_NPCC_2021)


Monthly_Aggregated_Filtered_Historic_NPCC_2021$Date =
  as.Date(paste("1" , Monthly_Aggregated_Filtered_Historic_NPCC_2021$Month_Year), format = "%d %B_%Y")

Monthly_Aggregated_Filtered_Historic_NPCC_2021 =
  Monthly_Aggregated_Filtered_Historic_NPCC_2021[order(Monthly_Aggregated_Filtered_Historic_NPCC_2021$Date), ]

# 
# Monthly_Mean_Lahore_Apparent_Temperature_Metrics =
#   forecast::accuracy(Predicted_Apparent_Temperature, Monthly_Aggregated_Filtered_Historic_NPCC_2021$NPCC_Monthly_Mean_Lahore_Apparent_Temperature_C)
}

# #




## Heat Map 

# m <- cor(Historic_NPCC_2016_2020[, which( tmp == "numeric" )], 
#          use = "complete.obs")
# m <- multicolenarity_check
# m <- tmp


m <- Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,c(2,3,5,6)]
colnames(m) = c("Monthly Mean Apparent temperature", "Monthly Min Apparent temperature","Monthly Max Apparent temperature", "Monthly Energy MWh")
m <- cor(m)
fig <- plot_ly(

  x = colnames(m),
  y = colnames(m),
  z = m, type = "heatmap"

)
# fig%>%
#   add_annotations(
#                   text = matrix(c(1,2,3,4,5,6,7,8), nrow=4, ncol=4, byrow = TRUE), 
#                   showarrow = FALSE)


m <- Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,c(2,3,5,4)]
colnames(m) = c("Monthly Mean Apparent temperature", "Monthly Min Apparent temperature","Monthly Max Apparent temperature", "Monthly Peak Power MW")
m <- cor(m)
fig <- plot_ly(

  x = colnames(m),
  y = colnames(m),
  z = m, type = "heatmap"

)
fig
# 
# d = rbind(Monthly_Aggregated_Filtered_Historic_NPCC_2016_2020[,c(7,6,4)], Monthly_Aggregated_Filtered_Historic_NPCC_2021[,c(5,4,2)])
# fig8 <- plot_ly(d,
#                x = d$Date, 
#                y = d$NPCC_Monthly_Energy_MWh,
#                type="scatter", mode="lines",
#                hovertemplate = paste(
#                  'Total month energy: ',d$NPCC_Monthly_Energy_MWh, 'MWh', 
#                  '<br>', months(as.Date(d$date))))
# x_axis_8 <- list(title='Months')
# y_axis_8 <- list(title = 'Monthly Energy (MWh)')
# title_8 <- (list="Monthly Energy Analysis")
# fig8 <- fig8 %>% layout(xaxis = x_axis_8, yaxis = y_axis_8)
# fig8
# 
# fig8 <- plot_ly(d,
#                               x = d$Date, 
#                               y = d$NPCC_Monthly_Peak_Power_MW,
#                               type="scatter", mode="lines",
#                               hovertemplate = paste(
#                                 'Total Peak Power: ',d$NPCC_Monthly_Peak_Power_MW, 'MWh', 
#                                 '<br>', months(as.Date(d$date))))
# x_axis_8 <- list(title='Months')
# y_axis_8 <- list(title = 'Monthly Peak Power (MW)')
# title_8 <- (list="Peak Power Analysis")
# fig8 <- fig8 %>% layout(xaxis = x_axis_8, yaxis = y_axis_8)
# fig8



