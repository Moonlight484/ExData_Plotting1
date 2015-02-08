plot3<- function() {
 ## read in the date, time and the 3 Sub_metering columns
 ## replace "   ?" with NA     
      df <- read.table(".\\exdata-data-household_power_consumption\\household_power_consumption.txt", 
                     sep=";", stringsAsFactors = FALSE,header=TRUE, na.strings="?",      
                     colClasses=c(rep("character",2),rep("NULL",4),rep("numeric",3))) 

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
     
## select the 3 sub_metering data as the  y-axis data 
      plot3Data <- reduced[c("Sub_metering_1","Sub_metering_2","Sub_metering_3")]

## set up the plotting into the figures directory
      if(!file.exists("figure")) {
        dir.create(figure)
      }
## Set up the plot to get as close to the example as possible
      png(filename = ".\\figure\\plot3.png", width = 480, height = 480, 
            units = "px", type = "cairo-png",bg = "white")

      with(plot3Data,plot(datetime,Sub_metering_1, xlab = "", ylab = "Energy sub metering", col="black",type = "n"))
      with(plot3Data, lines(datetime,Sub_metering_1, type="l"))
      with(plot3Data,lines(datetime,Sub_metering_2, col="red"))
      with(plot3Data,lines(datetime,Sub_metering_3, col = "blue"))
      legend("topright",lwd=1, cex=.95, col=c("black","red","blue"), 
            legend = c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"))
   
    dev.off()
   
   
   
   ##   reduced <- df[selRows,]
   ##   plot1Data <- as.numeric(reduced$Global_active_power)
   
   
}