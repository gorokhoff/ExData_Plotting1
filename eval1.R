library(sqldf)
File=file("household_power_consumption.txt")
df=read.csv.sql("household_power_consumption.txt",
                "select * from File where Date in ('2/1/2007', '2/2/2007')", 
                header=TRUE, sep= ";")
png(file = "plot22.png")
dftd <- paste(df$Date,df$Time,sep = " ")
dft <- strptime(dftd,format = "%m/%d/%Y %H:%M:%S")
with(df,plot(x = dft,y = Global_active_power,type = "l",
             ylab = "Global Active Power(kilowatts)" ,xlab = " "))
dev.off()