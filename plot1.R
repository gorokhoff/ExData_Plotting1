#Load data
df<-read.table("household_power_consumption.txt",header = TRUE, sep = ";", na.strings = "?")
#convert Date
df$Date<-as.Date(df$Date, "%d/%m/%Y")
#subset
dfs<-df[(df$Date >= as.Date("2007-02-01", "%Y-%m-%d") & df$Date <= as.Date("2007-02-02", "%Y-%m-%d")),]
#create histogram
png(filename="plot1.png")
hist(dfs$Global_active_power, col = "red", xlab = "Global active power (killowats)", main = "Global active power")
dev.off()
