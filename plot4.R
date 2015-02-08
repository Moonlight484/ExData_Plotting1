plot4<- function() {

## read in the all the data
## replace "   ?" with NA
      df <- read.table(".\\exdata-data-household_power_consumption\\household_power_consumption.txt", 
                     sep=";", stringsAsFactors = FALSE,header=TRUE, na.strings="?")
      
## change the data column into a formatted Date
      df$Date <- as.Date(df$Date,format = "%d/%m/%Y")

## remove any rows where there are NAs
      dfc <-complete.cases(df)
      cleandf <- df[dfc,]

## put the date and time together so you can convert it
      alldatetime <-paste(cleandf$Date, cleandf$Time, sep=':')

## convert it to a POSIX date/time and select the dates 
## --on or after 02-01 and before 02-03
      mydate <- strptime(alldatetime,format="%Y-%m-%d:%H:%M:%S")
      selRows <- which(mydate >= as.POSIXlt("2007-02-01") & 
                             mydate < as.POSIXlt("2007-02-03" ))

## select these desired rows from the data, this will be the X-axis
      datetime <-mydate[selRows]

## select the rows from the cleaned up data frame - this will be the data to plot
      reduced <- cleandf[selRows,]
            
 
     ## set up the plotting into the figures directory
     if(!file.exists("figure")) {
           dir.create(figure)
     }
     
     ## Set up the plot to get as close to the example as possible
      png(filename = ".\\figure\\plot4.png", width = 480, height = 480, 
          units = "px", type = "cairo-png",bg="white")
      par(mfrow=c(2,2))

     
## plot [1,1]      
## Get the Global_active_power column and plot it against datetime
## with no x-axis label and a label on the y-axis ("Global_active_power")
##data for plot 4.1
      plot1Data <- as.numeric(reduced$Global_active_power) 

## add the plot to the [1,1] position
      plot(datetime,plot1Data,type="n",main="", bg="white",
     
         xlab = "",  ylab="Global Active Power")

      lines(datetime,plot1Data,type="l")

## plot [1,2] 
## Get the voltage column and plot it against datetime with a label on the 
## x-axis (default) and the y-axis ("Voltage")
##data for plot 4.2
      plot2Data <- as.numeric(reduced$Voltage)   
## add the plot to the [1,2] position
      plot(datetime,plot2Data,type="n",main="",  bg="white",
     
            ylab="Voltage")

      lines(datetime,plot2Data,type="l")

##plot[2,1]  

## get all the data for the sub_metering columns and plot them on one plot
## with a legend, no x-axis label and a y-axis label ("Energy sub metering")
## data for plot 4.3
      plot3Data <- cleandf[selRows,
            c("Sub_metering_1","Sub_metering_2","Sub_metering_3")] 
## add the plot to the [2,1] position
      with(plot3Data,plot(datetime,Sub_metering_1, xlab = "", 
            ylab = "Energy sub metering", col="black",type = "n", bg="white"))
      with(plot3Data, lines(datetime,Sub_metering_1, type="l"))
      with(plot3Data,lines(datetime,Sub_metering_2, col="red"))
      with(plot3Data,lines(datetime,Sub_metering_3, col = "blue"))
## add the legend
      legend("topright",lwd=1, cex=.95, bty = "n", 
             col=c("black","red","blue"), 
            legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))



##plot[2,2]
## Get the Global_reactive_power column and plot it against datetime 
## with a label on the x-axis (default) and the y-axis ("Global_reactive_power")
## data for plot 4.4
      plot4Data <- as.numeric(reduced$Global_reactive_power)   
## add the plot to the [2,2] position
      plot(datetime,plot4Data,type="n",main="", bg="white",
            ylab="Global_reactive_power")

      lines(datetime,plot4Data,type="l")
   
## close the device and write the file 
      dev.off()
   
   
   
   
   
}