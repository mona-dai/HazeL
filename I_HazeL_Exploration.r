#### HazeL Data Exploration Tutorial ####
## before you try this code: 
#### go through read.hazel.beta.R and save iti in your working directory
#### make sure you have both the data file and meta file in your directory. 
#### read.hazel.beta.R will look for "data" in your filename and find the matching "meta" file. 

#### 1. First WD and load packages####
  
  setwd("/Users/byy635/OneDrive - Harvard University/Teaching/ESE 6/Labs/Bryan's data/")
  library(lubridate)
  source("read.hazel.beta.r") #make sure you have read.hazel.beta.r in your working directory
  

#### 2. Read in and time zone change ####
  dum<-read.hazel("220204_201000_data.txt") ## run my file in the hazel function and save it as a new object
  dum$ETC_timestamp<-with_tz(dum$UTC_timestamp, tz="EST") ## I like to work in EST, so let's make a new column. But do not overwrite the UTC
  
  colnames(dum)[length(dum)]<-"EST_timestamp" # new column is the last column since it's new. length(dum) will give you the number of the columns, therefore also the last column

####3. data exploration ####
  #a) plotting
    plot(dum$EST_timestamp, dum$X0.3um, type = "l") 
    ### I don't like the x axis inteval.

  #b) with better x-axis
    plot(dum$EST_timestamp, dum$X0.3um, type = "l", xlab="Date/Time",xaxt="n", ylab="0.3 um") # plotting without the x axis label

    # adding a new x axis label at the interval I want. the format is what I want to show. you can also do %H:%M if you have shorter data.
    axis.POSIXct(1, at=seq(range(dum$EST_timestamp)[1], range(dum$EST_timestamp)[2], by="2 hours"), format="%m/%d %H:%M")
    legend("topleft", "TZ = EST") #Time zone is important as well.
    ## I still don't like the axis. 

  #c) x-axis now only showing the hours.
    plot(dum$EST_timestamp, dum$X0.3um, type = "l", xlab="Date/Time",xaxt="n", ylab="0.3 um particle count") # plotting without the x axis label
    tickstart<-ceiling_date(range(dum$EST_timestamp)[1], unit = "hours") ##rouding up the first timestamp to the nearest hour
    tickend<-floor_date(range(dum$EST_timestamp)[2], unit = "hours") ##rounding down the first timestamp to the nearest hour

    # adding a new x axis label at the interval I want. the format is what I want to show. you can also do %H:%M if you have shorter data.
    axis.POSIXct(1, at=seq(tickstart,tickend, by="1 hour"), format="%m/%d %H:%M")
    legend("topright", "TZ = EST", bty="n") #Time zone is important as well.


#### 4. Averaging the data for specific time points (for calibration or reporting the average of a location/activity)	
  #a.What was the particle count from 1-2 am when there was no activity?
    # I need to first isolate all the data that was collected between 1 and 2 AM
    time_window<-which(floor_date(dum$EST_timestamp,unit= "hours")=="2022-02-05 01:00:00 EST")
    ## the above code first rounds down the time stamp to the nearest hour, and see which ones show 1 AM. 
    ## object time_window is the rows that would show 1 AM when the timestamp column is rounded down.

    range(dum$EST_timestamp[time_window]) ## the function range shows the minimum and maximum of a vector.
    # it looks like the index vector "time_window" is capturing all the data collected between 1 and 2 am.


  #b. Let's calculate the mean and the standard deviation of the 1-2 am period
    mean(dum$X0.3um[time_window]) 
    sd(dum$X0.3um[time_window])

  #c. Can I use this to find the HazeL data i need for calibration? You bet.
    calibration1<-which(floor_date(dum$EST_timestamp,unit= "mins")=="2022-02-04 17:55:00 EST")
    ## the above line now rounds down all the timestamp to the nearest min and see which one matches 17:55
    range(dum$EST_timestamp[calibration1])
    ## the range checks out!

    mean(dum$X0.3um[calibration1]) 
    ## the mean from 17:55-17:56. 

  #d. What if I want 30 seconds data?


    calibration2<-which(floor_date(dum$EST_timestamp,unit= "30 secs")=="2022-02-04 17:55:00 EST")
    ## the above line now rounds down all the timestamp to the nearest 30 seconds and see which one matches 17:55
    range(dum$EST_timestamp[calibration2])
    ## the range checks out!

    mean(dum$X0.3um[calibration2]) #Qapla! (means success in Klingon)
    

    
#### 5. Calibration ####
  # calibration - I am going to use my file to demonstrate how calibration works. But please use your own calibration files once you get the hang of this:
 
  #a.calibration data from the HazeL data and TSI
    # my three points of calibration are 16:12, 18:45, 21:21(I will use the one minute average)
    cal.time<- as.POSIXct(c("2022-02-04 16:12:00 EST","2022-02-04 18:45:00 EST","2022-02-04 21:21:00 EST"))
    
    
    # first, making a filter for separating the specific data I want.
    calibration1<-which(floor_date(dum$EST_timestamp,unit= "mins")=="2022-02-04 16:12:00 EST")
    calibration1 ## 1424 to 1446th rows in my file cover the 16:12-16:13 period
    cal1_mean<-mean(dum$X0.3um[calibration1]) ## mean is 1287.783

    calibration2<-which(floor_date(dum$EST_timestamp,unit= "mins")=="2022-02-04 18:45:00 EST")
    calibration2 ## 
    cal2_mean<-mean(dum$X0.3um[calibration2]) ## mean is 4905.348
    
    calibration3<-which(floor_date(dum$EST_timestamp,unit= "mins")=="2022-02-04 21:21:00 EST")
    calibration3 ## 
    cal3_mean<-mean(dum$X0.3um[calibration3]) ## mean is 1711.864
    
    ## the unit is counts per 0.1 liter per sec, so lets convert to 1 L
    X0.3umperL<-c(cal1_mean, cal2_mean, cal3_mean) *10
    #
    
    # let's make a new vector with the TSI data.
    # TSI data is cumulative counts over 25 sec at 2.83 L per minute flow, let's convert to counts per sec per Liter
    tsi.data<-c(16230,53042,18930)/25*60/2.83 # now in counts per second per liter.
    
  #b. Combining into a data frame.
    cal.table<-data.frame(cal.time, tsi.data,X0.3umperL)
    str(cal.table) ## making sure that I have three columns with time, numeric, numeric
    
    
  #c. Let's see if the calibration looks ok
    plot(cal.table$X0.3umperL, cal.table$tsi.data) 
    ## I wish I had more calibration points.
    lm.cal<- lm(tsi.data~X0.3umperL, data=cal.table) ## linear regression between the hazel mean data and tsi data for the three calibration points
    summary(lm.cal) # what do think this does?
    
  #d. Applying the linear regression to create a new calibrated data output  
    dum$X0.3umperL<-dum$X0.3um*10 # our regression assume hazel count in liters
     predict(lm.cal,dum) ## this is the calibrated data. 
   
    
    plot(dum$EST_timestamp,predict(lm.cal,dum), type="l", col="red", ylim = c(0,200000), ylab= "0.3um count L-1 S-1", xlab="time" )
    par(new=t) # this line lets you overlay a new plot on the existing plot, make sure you have defined ylim and xlim
    plot(dum$EST_timestamp,dum$X0.3umperL, type="l", col="blue", ylim = c(0,200000), xaxt='n', yaxt='n', xlab="",ylab="") 
    
    ## You now see calibrated data as blue.You can also add the calibrated data as a new column.
    
    