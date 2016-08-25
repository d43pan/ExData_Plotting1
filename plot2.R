# Line graph
# no title
# no x axis label
# y axis label - Global Active Power (kilowatts)
# line color - black
# x axis ticks - days of week

processed_filename = "household_power_consumption_2017-02-01--2017-02-02.csv"

get_data <- function(){
  library(dplyr)
  library(lubridate)
  
  if( !file.exists(processed_filename)){
    power_subset <- tbl_df( read.csv("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?") ) %>%
      mutate(Date = as.Date(Date, format = '%d/%m/%Y')) %>%
      subset( ( Date == as.Date('2007-02-01') | Date == as.Date('2007-02-02')))
  
    power_subset$datetime <- with(power_subset,  ymd_hms( paste(Date, Time) ) ) 
    
    write.csv(power_subset, file=processed_filename, row.names = FALSE)
  
  }
    colClasses=c("Date"="Date", "datetime"="POSIXct")
    power_subset <-  tbl_df( read.csv(processed_filename, header = TRUE, colClasses = colClasses))
    power_subset

}

plot2 <- function(){
  power_consumption_subset <- get_data()
  
  png(filename = "plot2.png",
      width = 480, 
      height = 480)
  
  plot.new()
  plot( x = power_consumption_subset$datetime,
        y = power_consumption_subset$Global_active_power,
        type="l", 
        ylab = "Global Active Power (kilowatts)",
        xlab = "")
  
  dev.off()
}