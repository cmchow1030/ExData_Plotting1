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
png(filename = "plot2.png", width = 480, height = 480)
with(subset_data, plot(Datetime, Global_active_power, type = "l", xaxt = "n", 
                       xlab = "", ylab= "Global Active Power (kilowatts)") )
time_obj <- as.POSIXct.POSIXlt(subset_data$Datetime)
axis(1, at = time_obj[c(1, 1441, 2881)], 
     labels =  strftime(time_obj[c(1, 1441, 2881)], format = "%a"))
dev.off()
