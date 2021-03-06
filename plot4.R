plot4 = function() {
  print ("Reading data.  This will take a few minutes.")
  z=getData("household_power_consumption.txt", "2007-02-01 00:00:00", "2007-02-02 24:00:00")
  print ("Read data has completed");
  
  png("plot4.png");
  par(mfrow=c(2,2))
  
  plot(z$DateTime, z$Global_active_power, type="n", 
       xlab="", ylab="Global Active Power (kilowatts)")
  lines(z$DateTime, z$Global_active_power )
  
  plot(z$DateTime, z$Voltage, type="n", 
       xlab="datetime", ylab="Voltage")
  lines(z$DateTime, z$Voltage )
  
  
  plot(c(min(zz$DateTime, na.rm=TRUE),max(zz$DateTime, na.rm=TRUE)), c(0,40), type="n", ylab="Energy sub metering", xlab=" ")
  lines(zz$DateTime, zz$Sub_metering_3, col="blue")
  lines(zz$DateTime, zz$Sub_metering_2, col="red")
  lines(zz$DateTime, zz$Sub_metering_1)
  legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black","red","blue"))
  
  plot(z$DateTime, z$Global_reactive_power, type="n", 
       xlab="datetime", ylab="Global_reactive_power")
  lines(z$DateTime, z$Global_reactive_power )
  
  dev.off();
  print ("Plot #4  in file plot4.png")
  
}

getData = function(filename, startDate, stopDate){
  con = file(filename, "r")
  NROWS=1000;
  colTypes= c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric","numeric","numeric")
  x=read.table(con, header=TRUE, sep=";", na.strings="?", nrows=NROWS, colClasses=colTypes)
  colnames = names(x);
  count=dim(x)[1]   ## number of rows read in last read
  total = count     ## cummulative rows read
  x$DateTime=strptime(paste(x$Date,x$Time,sep=" "), "%d/%m/%Y %H:%M:%S")
  x$Date=NULL
  x$Time=NULL
  x = x[x$DateTime >= startDate & x$DateTime <= stopDate, ]
  while (count >= NROWS ){
    y=read.table(con, header=FALSE, sep=";", na.strings="?", nrows=NROWS, col.names=colnames, colClasses=colTypes)
    count=dim(y)[1]
    total=total+count
    y$DateTime=strptime(paste(y$Date,y$Time,sep=" "), "%d/%m/%Y %H:%M:%S")
    y$Date=NULL
    y$Time=NULL
    y = y[y$DateTime >= startDate & y$DateTime <= stopDate, ]
    x=rbind(x, y)
    ##print( c(total, dim(x)[1]))
  }
  close(con)
  x
}