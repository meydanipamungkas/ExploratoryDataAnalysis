#data preprocessing
library(data.table)
power<-read.table("household_power_consumption.txt",skip=1,sep=";")
names(power)<-c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
power2<-subset(power,power$Date=="1/2/2007"|power$Date=="2/2/2007")

#histogram of the data
hist(as.numeric(as.character(power2$Global_active_power)),col="red",main="Global Active Power",xlab="Global Active Power(kilowatts)")

#title of the histogram
title(main="Global Active Power")
