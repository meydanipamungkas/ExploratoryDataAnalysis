#data preprocessing
library(read.table)
power<-read.table("household_power_consumption.txt",skip=1,sep=";")
names(power)<-c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
power2<-subset(power,power$Date=="1/2/2007"|power$Date=="2/2/2007")

#data transforming
power2$Date<-as.Date(power2$Date,format="%d/%m/%Y")
power2$Time<-strptime(power2$Time,format="%H:%M:%S")
power2[1:1440,"Time"]<-format(power2[1:1440,"Time"],"2007-02-01 %H:%M:%S")
power2[1441:2880,"Time"]<-format(power2[1441:2880,"Time"],"2007-02-02 %H:%M:%S")

#plot of the data
plot(power2$Time,as.numeric(as.character(power2$Global_active_power)),type="l",xlab="",ylab="Global Active Power (kilowatts)") 

#title of the plot
title(main="Global Active Power Vs Time")