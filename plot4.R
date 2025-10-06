read_data <- function (){
        data <- read.table("household_power_consumption.txt", sep = ";", 
                           header = TRUE, na.strings = "?")
        reformated_date <- as.Date(data$Date, format = "%d/%m/%Y")
        date_data <- cbind(reformated_date, data[,c(2:9)])
        subset_data <- subset(date_data, reformated_date == "2007-02-01"| 
                                      reformated_date == "2007-02-02" |reformated_date =="2007-02-03" & Time == "00:00:00")
        
        colnames(subset_data)[1] <- "Date"
        datetime_str <- paste(subset_data$Date, subset_data$Time)
        subset_data$Datetime <- strptime(datetime_str, format = "%Y-%m-%d %H:%M:%S")
        subset_data
}
subset_data <- read_data()
png(filename = "plot4.png", width = 480, height = 480)
par(mfcol = c(2, 2))
#graph 1
with(subset_data, plot(Datetime, Global_active_power, type = "l", xaxt = "n", 
                       xlab = "", ylab= "Global Active Power (kilowatts)") )
time_obj <- as.POSIXct.POSIXlt(subset_data$Datetime)
axis(1, at = time_obj[c(1, 1441, 2881)], 
     labels =  strftime(time_obj[c(1, 1441, 2881)], format = "%a"))
#graph 2
with(subset_data,{
        plot(Datetime,Sub_metering_1, type = "l", col = "black", lty = 1, 
             xaxt = "n", xlab = "", ylab = "Energy sub metering")        
        lines(Datetime, Sub_metering_2, col = "red", lty = 1)
        lines(Datetime, Sub_metering_3, col = "blue", lty = 1)
})
time_obj <- as.POSIXct.POSIXlt(subset_data$Datetime)
axis(1, at = time_obj[c(1, 1441, 2881)], 
     labels =  strftime(time_obj[c(1, 1441, 2881)], format = "%a"))
# Add a legend
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black", "red", "blue"), lty = 1)
#graph 3
with(subset_data, plot(Datetime, Voltage, type = "l", xaxt = "n", 
                        ylab= "Voltage") )
time_obj <- as.POSIXct.POSIXlt(subset_data$Datetime)
axis(1, at = time_obj[c(1, 1441, 2881)], 
     labels =  strftime(time_obj[c(1, 1441, 2881)], format = "%a"))
#graph4
with(subset_data, plot(Datetime, Global_reactive_power, type = "l", xaxt = "n", 
                        ylab= "Global_reactive_power") )
time_obj <- as.POSIXct.POSIXlt(subset_data$Datetime)
axis(1, at = time_obj[c(1, 1441, 2881)], 
     labels =  strftime(time_obj[c(1, 1441, 2881)], format = "%a"))
dev.off()