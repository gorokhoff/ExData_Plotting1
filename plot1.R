#df<-read.table("household_power_consumption.txt",header = TRUE, sep = ";", na.strings = "?")
df$Date<-as.Date(df$Date, "%d/%m/%Y")
dfs<-df[(df$Date >= as.Date("2007-02-01", "%Y-%m-%d") & df$Date <= as.Date("2007-02-02", "%Y-%m-%d")),]
hist(dfs$Global_active_power, col = "red", xlab = "Global active power (killowats)", main = "Global active power")

