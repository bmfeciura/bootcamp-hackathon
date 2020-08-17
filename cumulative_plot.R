# read in the csv
rawdata = read.csv("time_series_covid19_confirmed_global.csv")
population = read.csv("populations.csv")

rawdata$population <- population$Population

# create list of needed countries
countries <- c("US", "United Kingdom", "China", "Korea, South", "World")

#create a new row for the world totals
world <- c(c("", "World", 0, 0), colSums(rawdata[,5:211], na.rm = TRUE))
china <- c(c("", "China", 0, 0), colSums(rawdata[which(rawdata$Country.Region == "China"),5:211]))
     
# add the new row back into the table
data <- rbind(rawdata, world)
data <- rbind(data, china)

# convert to cases per million
#install.packages("reshape2")
library(reshape2)

data <- reshape2::melt(data, id.vars = c("Province.State", "Country.Region", "population","Lat", "Long"), variable.name = "Date", value.name = "Confirmed") # reshape data so it's easier to work with
data$Confirmed <- as.numeric(data$Confirmed)
data$population <- as.numeric(data$population, na.rm = TRUE)

data$Infectedpermil<-(1000000*data$Confirmed/data$population)



#subset for the desired countries
selection <- data[which(data$Country.Region %in% countries & data$Province.State == ""),]

#reformat the dates
selection$Date<-as.Date(selection$Date, format = c("X%m.%d.%y"))

library(ggplot2)

#build a line plot
plot <- ggplot(selection, aes(y = Infectedpermil, x = Date, fill = Country.Region, color = Country.Region)) +  geom_line()
plot <- plot + scale_y_continuous(trans='log10', breaks = c(.01, .1, 1, 10, 100, 1000, 10000, 100000), labels = c(.01, .1, 1, 10, 100, 1000, 10000, 100000)) #with logarithmic y-axis
plot <- plot + scale_x_date(date_labels="%y/%m", date_breaks="4 weeks")
plot <- plot + labs(title = "Total Confirmed COVID-19 Cases per million people", x = "", y = "LOG")
plot <- plot + theme(legend.title=element_blank())
plot