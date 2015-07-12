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
png(filename="plot2.png")
Sys.setlocale("LC_TIME", "English")
plot(dfs$DateTime, dfs$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.off()
Sys.setlocale("LC_TIME", "Russian")
