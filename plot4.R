#Load data
df<-read.table("household_power_consumption.txt",header = TRUE, sep = ";", na.strings = "?")
#convert Date
df$Date<-as.Date(df$Date, "%d/%m/%Y")
#subset
dfs<-df[(df$Date >= as.Date("2007-02-01", "%Y-%m-%d") & df$Date <= as.Date("2007-02-02", "%Y-%m-%d")),]
#Create DateTime column
datetime <- paste(as.Date(dfs$Date), dfs$Time)
dfs$DateTime <- as.POSIXct(datetime)
#create plot
Sys.setlocale("LC_TIME", "English")
png(filename="plot4.png")
par(mfrow = c(2,2))
plot(dfs$DateTime, dfs$Global_active_power, type="l", xlab="", ylab="Global Active Power")
plot(dfs$DateTime, dfs$Voltage, type="l", xlab="", ylab="Voltage")
plot(dfs$DateTime, dfs$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
lines(dfs$DateTime, dfs$Sub_metering_2, type="l", xlab="",  col = "red")
lines(dfs$DateTime, dfs$Sub_metering_3, type="l", xlab="",  col = "blue")
legend("topright", lwd = 1, col=c("black","red", "blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
plot(dfs$DateTime, dfs$Global_reactive_power, type="l", xlab="", ylab="Global Reactive Power")
dev.off()
Sys.setlocale("LC_TIME", "Russian")
