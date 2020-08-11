#Load Dataset and take a look on the snapshot:
getwd()
setwd("/Users/test/Desktop/Rcode")
dir()
NEI <- readRDS("exdata_data_NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata_data_NEI_data/Source_Classification_Code.rds")
head(NEI)
str(NEI)
head(SCC)
str(SCC)
#total PM2.5 emission 
totalpm25_emission <- aggregate(Emissions ~ year, data = NEI, sum)
##plot 1 :
color <- c("Red", "Orange", "Blue", "Green")
barplot(height = totalpm25_emission$Emissions / 1000, 
        names.arg = totalpm25_emission$year, xlab= 'Year', ylab = expression('Aggregated Emissions'), 
        main=expression('Aggregated PM'[2.5]*' Emmissions by Year'), col = color)
#Baltimore City data:
balcity <- NEI[NEI$fips =="24510", ]
balcity_emission <- aggregate(Emissions ~ year , data = balcity, sum)
#plot 2:
color <- c("Red", "Orange", "Blue", "Green")
barplot(height = balcity_emission$Emissions / 1000, 
        names.arg = balcity_emission$year, xlab= 'Year', ylab = expression('Aggregated Emissions'), 
        main=expression('Aggregated PM'[2.5]*' Emmissions by Year'), col = color)
#Baltimore City :
BalYrTypEm <- aggregate(Emissions ~ year + type, data = balcity, sum)
#plot 3:
library(ggplot2)
chart <- ggplot(BalYrTypEm, aes(year, Emissions, col = type))
chart <- chart + geom_line() +xlab("year") +
    ylab(expression('Total Emissions')) +
    ggtitle('Total Baltimore Emissions [2.5]* From 1999 to 2008')
print(chart)
#plot 4:
plot4 <- ggplot(BalYrTypEm, aes(factor(year), Emissions))
plot4 <- chart + geom_bar(stat="identity") +
    xlab("year") +  
    ylab(expression('Total Emissions')) +
    ggtitle('Total [2.5]* Coal Emissions From 1999 to 2008')
print(plot4)

#plot 5 :
# Type: ON-ROAD, Fips = "24510" Baltimore Motor Vehicle PM[2.5]* Emissions from 1999 to 2008
plot5 <- ggplot(BalYrTypEm, aes(factor(year), Emissions))
plot5 <- plot5 + geom_bar(stat = 'identity') +
    xlab("year") +  
    ylab(expression('Total Emissions')) +
    ggtitle('Baltimore Motor Vehicle PM[2.5] Emissions From 1999 to 2008')
print(plot5)

#Baltimore, MD-24510 and Los Angeles, CA-06037
library(dplyr)
BalYrTypEmFltr <- summarise(group_by(filter(NEI, NEI$fips == "24510"& type == 'ON-ROAD'), year), Emissions=sum(Emissions))
LaYrTypEmFltr <- summarise(group_by(filter(NEI, NEI$fips == "06037" & type == 'ON-ROAD'), year), Emissions=sum(Emissions))    

BalYrTypEmFltr$County <- 'Baltimore City, MD'
LaYrTypEmFltr$County <- 'Los Angeles, CA'
BalLaEm <- rbind(BalYrTypEmFltr, LaYrTypEmFltr)

# plot6:
# Type: ON-ROAD, Fips = 24510 for Baltimore, MD Motor Vehicle PM[2.5]* Emissions Against Los Angeles, CA Fips = 06037  from 1999 to 2008
plot6 <- ggplot(BalLaEm, aes(x=factor(year), y=Emissions, fill=County,label = round(Emissions,2)))
plot6 <- plot6 + geom_bar(stat="identity") +
    facet_grid(County~., scales="free") +
    ylab(expression("total PM"[2.5]*" emissions in tons")) + 
    xlab("year") +
    ggtitle(expression("Baltimore City vs Los Angeles Motor vehicle emission in tons"))+
    geom_label(aes(fill = County),colour = "blue", fontface = "bold")
print(plot6)

