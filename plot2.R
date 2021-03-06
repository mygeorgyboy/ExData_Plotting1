rm(list=setdiff(ls(), "x"))
rm(x)
setwd("/Users/jramirez/Documents/proyectosR/ExploratoryDataAnalysis")
getwd()

ElectricData <- read.csv("./household_power_consumption.txt",header = TRUE,sep =";",nrows = 548000)
ElectricData$Date <- as.Date(ElectricData$Date, format="%d/%m/%Y")

ElectricData <-ElectricData[(ElectricData$Date=="2007-02-01") | (    ElectricData$Date=="2007-02-02"),]
ElectricData$Global_active_power <- as.numeric(as.character(ElectricData$Global_active_power))
ElectricData$Global_reactive_power <- as.numeric(as.character(ElectricData$Global_reactive_power))
ElectricData$Voltage <- as.numeric(as.character(ElectricData$Voltage))
ElectricData <- transform(ElectricData, timestamp=as.POSIXct(paste(Date, Time)), "%d/%m/%Y %H:%M:%S")
ElectricData$Sub_metering_1 <- as.numeric(as.character(ElectricData$Sub_metering_1))
ElectricData$Sub_metering_2 <- as.numeric(as.character(ElectricData$Sub_metering_2))
ElectricData$Sub_metering_3 <- as.numeric(as.character(ElectricData$Sub_metering_3))

plot(ElectricData$timestamp,ElectricData$Global_active_power, type="l", xlab="", ylab="Global Active Power (kilowatts)")
dev.copy(png, file="plot2.png", width=480, height=480)
dev.off()

