plot1 = function(){
  print ("Reading data.  This will take a few minutes.")
  z=getData("household_power_consumption.txt", "2007-02-01 00:00:00", "2007-02-02 24:00:00")
  print ("Read data has completed");
  png("plot1.png");
  hist(z$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", 
       main="Global Active Power")
  dev.off();
  print ("Plot #1  in file plot1.png")
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