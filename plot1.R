# Histogram
# title - Global Active Power
# x axis - Global Active Power (kilowatts)
# y axis - Frequency
# bar color - red


get_data <- function(){
  library(dplyr)
  library(lubridate)
  
  power_subset <- tbl_df( read.csv("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?") ) %>%
    mutate(Date = as.Date(Date, format = '%d/%m/%Y')) %>%
    subset( ( Date == as.Date('2007-02-01') | Date == as.Date('2007-02-02')))
  
  power_subset$datetime <- with(power_subset,  ymd_hms( paste(Date, Time) ) ) 
  
  power_subset
}

plot1 <- function(){
power_consumption_subset <- get_data()

png(filename = "plot1.png",
    width = 480, 
    height = 480)


hist(power_consumption_subset$Global_active_power, 
     breaks = 15, 
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency",
     main = "Global Active Power",
     col = "red"
     
     )


dev.off()
}