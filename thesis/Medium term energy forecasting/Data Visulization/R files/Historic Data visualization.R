library('ggplot2')
library('tidyr')
library('sqldf')
library('plotly')

q <- read.csv('Historic_NPCC_Load_Forecast_Weather_OLD.csv', header = TRUE)

q$Time <- as.POSIXct(q$Time) 
#plt<-ggplot(data = q) + geom_line(mapping = aes(x=Time, y=hourly_load), color='#00AFBB') 
#print(plt)
fig<-plot_ly(q, x=q$Time, y=q$hourly_load, mode='line', type='scatter')
fig

#hist(q$hourly_load, main ="Hourly load frequencies") 
fig1<- plot_ly(x=q$hourly_load, type = 'histogram')
fig1
## splitting 
dt<-tidyr::separate(q, Time, c("date", "time"), sep = " ")
hms<-tidyr::separate(dt, time, c("hour", "min", "secs"), sep = ":", remove=FALSE)
ymd<-tidyr::separate(hms, date, c("year", "month", "day"), sep = "-", remove=FALSE)
ymd$weekdays <- weekdays(as.Date(ymd$date,'%Y-%m-%d'))

## stats on basis of months
#ggplot() +geom_point(data =ymd,aes(x=month, y=hourly_load, color=month))  ## remove
ggplot() +geom_boxplot(data =ymd,aes(x=month, y=hourly_load, color=month), size=1)
fig2<- plot_ly(ymd,y= ymd$hourly_load, color=ymd$month ,type='box')
fig2

monthly_load_stats = sqldf('SELECT month, max(hourly_load) as max_load, min(hourly_load) as min_load, avg(hourly_load) as avg_load from ymd group by month')
ggplot() +geom_line(data =monthly_load_stats,aes(x=month, y=max_load, color='maximum hourly load', group=1), size=1) +geom_line(data =monthly_load_stats, aes(x=month, y=min_load,color='minimum hourly load', group=1), size=1) +geom_line(data =monthly_load_stats, aes(x=month, y=avg_load, color='average hourly load', group=1),size=1)
fig3 <- plot_ly(monthly_load_stats, x = monthly_load_stats$month) 
fig3 <- fig %>% add_trace(y =monthly_load_stats$max_load, name = 'trace 0',mode = 'lines')
fig3 <- fig %>% add_trace(y =monthly_load_stats$min_load, name = 'trace 1',mode = 'lines')
fig3 <- fig %>% add_trace(y =monthly_load_stats$avg_load, name = 'trace 2',mode = 'lines')
fig3

## stats on basis of days
daily_data_of_month = sqldf("SELECT * from ymd where month = '08'") ## can change month on your own by specifying month no.
ggplot() +geom_point(data =daily_data_of_month,aes(x=day, y=hourly_load, color=day), size=1)
ggplot() +geom_boxplot(data =daily_data_of_month,aes(x=day, y=hourly_load, color=day), size=1)
daily_load_stats = sqldf("SELECT day, max(hourly_load) as max_load, min(hourly_load) as min_load, avg(hourly_load) as avg_load from ymd where month = '12' group by day")
ggplot() +geom_point(data =daily_load_stats,aes(x=day, y=max_load, color='maximum hourly load'), size=3) +geom_point(data =daily_load_stats, aes(x=day, y=min_load,color='minimum hourly load'), size=3) +geom_point(data =daily_load_stats, aes(x=day, y=avg_load, color='average hourly load'),size=3)

## all days line plot

ggplot(daily_data_of_month,aes(x=hour, y=hourly_load,color=day ,group = day)) +geom_line( size=1)+scale_color_hue(l=50, c=170)


## stats on basis of hours

hourly_data_of_day = sqldf("SELECT * from ymd where month = '02' and day='06'")  ## can change month and day
ggplot(hourly_data_of_day,aes(x=hour, y=hourly_load,group = 1)) +geom_line(size=1)




## line plot

daily_data_of_month = sqldf("SELECT * from ymd where month = '08'") ## can change month#
#fig5<- plot_ly(daily_data_of_month, x=daily_data_of_month$hour, y= daily_data_of_month$hourly_load, color=daily_data_of_month$day, group=daily_data_of_month$day ,type='scatter', mode='line')  ## for all days of specific month
#daily_data_of_month = sqldf("SELECT day, weekdays, avg(hourly_load) as avg_day_load from ymd group by weekdays")

fig5<- plot_ly(daily_data_of_month, x=daily_data_of_month$hour, y= daily_data_of_month$hourly_load, color=daily_data_of_month$weekdays ,type='scatter', mode='line')  ## for all days of specific month

x_axis_5 <- list(title='Hours')
y_axis_5 <- list(title = 'hourly_load')
title_5 <- (list="Line Plot for Days' energy load")

fig5 <- fig5 %>% layout(xaxis = x_axis_5, yaxis = y_axis_5, title=title_5)
fig5

