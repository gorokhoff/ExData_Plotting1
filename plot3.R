#Load data
df<-read.table("household_power_consumption.txt",header = TRUE, sep = ";", na.strings = "?")
#convert Date
df$Date<-as.Date(df$Date, "%d/%m/%Y")
#subset
dfs<-df[(df$Date >= as.Date("2007-02-01", "%Y-%m-%d") & df$Date <= as.Date("2007-02-02", "%Y-%m-%d")),]
#Create DateTime column
datetime <- paste(as.Date(dfs$Date), dfs$Time)
dfs$DateTime <- as.POSIXct(datetime)
#Create and save plot
png(filename="plot3.png")
plot(dfs$DateTime, dfs$Sub_metering_1, type="l", xlab="", ylab="Energy sub metering")
lines(dfs$DateTime, dfs$Sub_metering_2, type="l", xlab="",  col = "red")
lines(dfs$DateTime, dfs$Sub_metering_3, type="l", xlab="",  col = "blue")
legend("topright", lwd = 1, col=c("black","red", "blue"), legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))
dev.off()
