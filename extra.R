#Skip reading (takes a long time) if file has already been read
if (!exists("HaveData")) {
#Read input data from file (assumed to be in the current directory)
    x<-readRDS("summarySCC_PM25.rds")
    HaveData <-T
}

#Years to plot
years<-c(1999,2002,2005,2008)
x$year<-as.factor(x$year)

#Subset input data selecting only PM25 and years in range
y<-subset(x,Pollutant=="PM25-PRI" & year %in% years)

#Aggregate by <year>
v<-tapply(y$Emissions,y$year,sum)

#Generate plot
png(file="plot1.png")
barplot(v/1E06,col="red",ylab="Mega Tonnes",xlab="Year",main="United States PM2.5 Emissions (All Sources)")
dev.off()

#Using gpplot2 and tidyr libraries
library(ggplot2)
library(tidyr)

#Skip reading (takes a long time) if file has already been read
if (!exists("HaveData")) {
#Read input data from file (assumed to be in the current directory)
    x<-readRDS("summarySCC_PM25.rds")
    HaveData <-T
}

#Code for Baltimore City
BaltimoreCode="24510"

#Year range of interest
years<-c(1999,2002,2005,2008)
x$year<-as.factor(x$year)

#Subset input data by selecting only PM2.5 and years in range from Baltimore
y<-subset(x,Pollutant=="PM25-PRI" & fips==BaltimoreCode & year %in% years)

#Aggregate by <year> and <type>
v<-tapply(y$Emissions,list(y$year,y$type),sum)

#Reshape data frame from "wide" to "long" format
v2<-as.data.frame(v)
v2$Year<-as.factor(rownames(v2))
v1<-as.data.frame(gather(v2,Type,Emissions,1:4))

#Scale data
v1$Emissions<-as.numeric(v1$Emissions)/1E3

#Generate plot
png(file="plot3.png",width=780)
ggp<-ggplot(v1,aes(Year,Emissions,fill=Type,label=round(Emissions,digits=2)))+
    geom_bar(stat="identity")+
    facet_grid(.~Type)+
    labs(x="Year", y="Kilo Tonnes",title=expression("Baltimore Total PM"[2.5]*" Emissions by Source Type"))+
    guides(fill=F)+
    geom_smooth(aes(group=Type),method="lm",se=F)+
    geom_text(vjust=0)

print(ggp)
dev.off()

#Skip reading (takes a long time) if file has already been read
if (!exists("HaveData")) {
#Read input data from file (assumed to be in the current directory)
    x<-readRDS("summarySCC_PM25.rds")
    HaveData <-T
}
x1<-readRDS("Source_Classification_Code.rds")

#Year range of interest
years<-c(1999,2002,2005,2008)
x$year<-as.factor(x$year)

#Search data related to coal combustion by looking for a
#regular expression containing <fuel> <combustion> in $EI.Sector
#and <coal> in either $EI.Sector or $SCC.Level.Three

x2<-grepl("fuel .*comb.*",x1$EI.Sector,ignore.case=T) & (grepl(" coal$",x1$EI.Sector,ignore.case=T) | grepl(" coal$",x1$SCC.Level.Three,ignore.case = T))
#Data (and plot) can change since subset of SCCs
#can be extracted in multiple ways (and from multiple columns)

#SCC of interest (subject to above condition)
scc<-x1[x2,"SCC"]

#Subset input data by <year> and <SCC> (and PM2.5)
y<-subset(x,Pollutant=="PM25-PRI" & year %in% years & SCC %in% scc)

#Aggregate results
v<-tapply(y$Emissions,y$year,sum)

#Generate plot
png(file="plot4.png")
barplot(v/1E03,col="blue",ylab="Kilo Tonnes",xlab="Year",main=expression("United States PM"[2.5]*"Emissions (Coal Combustion)"))
dev.off()

library(ggplot2)
library(tidyr)

#Skip reading (takes a long time) if file has already been read
if (!exists("HaveData")) {
#Read input data from file (assumed to be in the current directory)
    x<-readRDS("summarySCC_PM25.rds")
    HaveData <-T
}
x1<-readRDS("Source_Classification_Code.rds")

#Year range of interest
years<-c(1999,2002,2005,2008)
x$year<-as.factor(x$year)

#fips codes for Baltimore and LA
BaltimoreCode="24510"
LACode="06037"

#Search data related to emission produced by vehicles by
#looking for a regular expression in column $EI.Sector

#Output data (and plot) can change since subset of SCCs
#can be extracted in multiple ways (and from multiple columns)
x2<-grep("vehicles",x1$EI.Sector,ignore.case=T)

#SCC of interest (related to vehicles)
scc<-x1[x2,"SCC"]

#Subset input data by <City>, <year> and <SCC>  - First part (Baltimore)
y1<-as.data.frame(subset(x,Pollutant=="PM25-PRI" &
                             fips == BaltimoreCode & year %in% years & SCC %in% scc))
y1$City="Baltimore"

#Subset input data by <City>, <year> and <SCC>  - Second part (LA)
y2<-as.data.frame(subset(x,Pollutant=="PM25-PRI" &
                             fips == LACode & year %in% years & SCC %in% scc))
y2$City="Los Angeles"

#Assemble complete matrix
y<-rbind(y1,y2)

#Aggregate by <City> and <Year>
v<-tapply(y$Emissions,list(y$year,y$City),sum)

#Reshape matrix from "wide" to "long" format
v2<-as.data.frame(v)
v2$Year<-as.factor(rownames(v2))
v1<-as.data.frame(gather(v2,City,Emissions,1:2))

#Scale output values
v1$Emissions<-as.numeric(v1$Emissions)/1E3

# Build Labels
#
# Except for the first year, each value will be compared to
# previous year (for the same city)

#Emissions for Baltimore
r1 <- v1[v1$City=="Baltimore","Emissions"]

#Emissions for LA
r2 <- v1[v1$City=="Los Angeles","Emissions"]

#Build a vector to compare Emissions
#Drop last value, and shift inserting a 0 for the first year (for each City)
#E.g 0,<1999 Baltimore Emissions>,...,0,<1999 LA Emissions> ...<2005 LA Emissions>
r <- c(0,r1[-length(r1)],0,r2[-length(r2)])

#
#Each bar will have a top label indicating the percentage of change
#compared to the previous year
v1$Label <- ifelse(r!=0, paste(sprintf("%+g",round((v1$Emissions-r)/r*100)),"%"), "")


#Generate plot
png(file="plot6.png", width=680)
ggp<-ggplot(v1,aes(Year,Emissions,fill=City))+
        geom_bar(stat="identity")+
    facet_grid(.~City)+
    labs(x="Year", y="Kilo Tonnes",title=expression("Vehicle-Related PM"[2.5]*" Emissions (Baltimore vs. LA)"))+
    guides(fill=F)+
    geom_text(vjust=0, aes(label=Label))
print(ggp)
dev.off()
