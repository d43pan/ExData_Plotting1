# Line graph
# no title
# no x axis label
# y axis label - Energy Sub Metering
# 3 Lines
# Line 1 - Sub_metering_1, color black
# Line 2 - Sub_metering_2, color red
# Line 3 - Sub_metering_3, color blue
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

plot3 <- function(){
  power_consumption_subset <- get_data()
  
  png(filename = "plot3.png",
      width = 480, 
      height = 480)
  
  plot.new()
  with( power_consumption_subset,  
        plot(datetime, Sub_metering_1,
        type="n", 
        ylab = "Energy Sub Metering",
        xlab = "")
  )
  
  with( power_consumption_subset, lines( datetime, Sub_metering_1, col="black") )
  with( power_consumption_subset, lines( datetime, Sub_metering_2, col="red") )
  with( power_consumption_subset, lines( datetime, Sub_metering_3, col="blue") ) 
  
  with( power_consumption_subset, legend("topright", 
                                        lty=c(1,1),
                                        col=c("black", "red", "blue"),
                                        legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  )
  
  dev.off()
}