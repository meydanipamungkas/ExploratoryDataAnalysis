#data preprocessing
power<-read.table("household_power_consumption.txt",skip=1,sep=";")
names(power)<-c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3")
power2<-subset(power,power$Date=="1/2/2007" | power$Date =="2/2/2007")

#data transforming
power2$Time<-strptime(power2$Time,format="%H:%M:%S")
power2[1:1440,"Time"]<-format(power2[1:1440,"Time"],"2007-02-01 %H:%M:%S")
power2[1441:2880,"Time"]<-format(power2[1441:2880,"Time"],"2007-02-02 %H:%M:%S")


#combine plot function
par(mfrow=c(2,2))

#plot of the data
with(power2,{
  plot(power2$Time,as.numeric(as.character(power2$Global_active_power)),type="l", xlab="",ylab="Global Active Power")  
  plot(power2$Time,as.numeric(as.character(power2$Voltage)),type="l",xlab="datetime",ylab="Voltage")
  plot(power2$Time,power2$Sub_metering_1,type="n",xlab="",ylab="Energy sub metering")
  with(power2,lines(Time,as.numeric(as.character(Sub_metering_1))))
  with(power2,lines(Time,as.numeric(as.character(Sub_metering_2)),col="red"))
  with(power2,lines(Time,as.numeric(as.character(Sub_metering_3)),col="blue"))
  legend("topright",lty=1,col=c("black","red","blue"),legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),cex = 0.6)
  plot(power2$Time,as.numeric(as.character(power2$Global_reactive_power)),type="l",xlab="datetime",ylab="Global_reactive_power")
})