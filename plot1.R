plot1 <- function() {
 ## read in the date, time and Global_active power columns
 ## replace "   ?" with NA
      
      df <- read.table(".\\exdata-data-household_power_consumption\\household_power_consumption.txt", 
         sep=";", stringsAsFactors = FALSE,header=TRUE, na.strings="?",
         colClasses=c(rep("character",3) ,rep("NULL",6))) 
      
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

## select the  Global_active_power as the yaxis        
      plot1Data <- as.numeric(reduced$Global_active_power)
## set up the plotting into the figures directory
      if(!file.exists("figure")) {
      dir.create(figure)
      }

## Set up the plot to get as close to the example as possible
      png(filename = ".\\figure\\plot1.png", width = 480, height = 480, 
          units = "px", type = "cairo-png",bg = "white")

## plot it   x-axis label  "Global Active Power (kilowatts)", y-axis=default
      hist(plot1Data, main="Global Active Power",
      xlab = "Global Active Power (kilowatts)", col = "red")
 ##close the device
      dev.off()
      
}