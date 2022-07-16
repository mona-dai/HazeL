
#### Script for reading in the Hazel File and cleaning up the time stamp###
# Make sure you have both the data file and meta file in your working directory ##
# This script will add the meta file data to the particle count data (meta data will be interpolated to match the intervals of the particle data timestamp)

read.hazel = function( datafile , n.data.cols = 11, n.meta.cols= 7) {
#
library(lubridate)
library(stringr)
		D0 = readLines( datafile, skipNul=T )
                c0 = count.fields( datafile , sep = ",")
		badlines = which( c0 !=  n.data.cols )
	        print(paste(length(badlines), " broken lines removed from the data file:", paste(badlines, collapse=" ")))	
		if( length(badlines) > 0) {
			D1 = D0 [ - badlines ]
				} else {
					D1 = D0
	         				}
		M1 = str_split( D1[2:length(D1)], pattern=",", simplify = T)
			#colClasses=c("integer","character",rep("numeric",9)) )
		dum1 = data.frame( as.integer( M1[ ,1]), M1[,2] ) 
				  for( k in 3:n.data.cols ) dum1 = cbind( dum1 , as.numeric( M1[,k]) ) 
		colnames(dum1) = unlist( str_split( D1[1], pattern = ",") )

 #use lubridate to convert.  HazeL output is apparently not close enough to POSIXct standard, and the time portion is not converted.
 	date.code1 <-as_datetime( dum1$UTC_timestamp )
 	dum1$UTC_timestamp = date.code1
		# metafile
	metafile = sub( "data", "meta", datafile)
	D0 = readLines( metafile, skipNul=T )
                c0 = count.fields( metafile , sep = ",")
		badlines = which( c0 !=  n.meta.cols )
	        print(paste(length(badlines), "broken lines removed from the meta file:", paste(badlines, collapse=" ")))	
		if( length(badlines) > 0) {
			D1 = D0 [ - badlines ]
				} else {
					D1 = D0
	         				}
		M1 = str_split( D1[2:length(D1)], pattern=",", simplify = T)
			#colClasses=c("integer","character",rep("numeric",9)) )
		dum2 = data.frame( as.integer( M1[ ,1]), M1[,2] ) 
				  for( k in 3:n.meta.cols ) dum2 = cbind( dum2 , as.numeric( M1[,k]) ) 
		colnames(dum2) = unlist( str_split( D1[1], pattern = ",") )


 #interpolate and merge

if(sum(! is.na(dum2[,"latitude"]))>2) { latitude = approx(x=dum2[,"ms"],y=dum2[,"latitude"], xout=dum1[,"ms"])$y } else { latitude=rep(NA,nrow(dum1))}
if(sum(! is.na(dum2[,"longitude"]))>2) { longitude = approx(x=dum2[,"ms"],y=dum2[,"longitude"], xout=dum1[,"ms"])$y } else { longitude=rep(NA,nrow(dum1))}
if(sum(! is.na(dum2[,"altitude"]))>2) { altitude = approx(x=dum2[,"ms"],y=dum2[,"altitude"], xout=dum1[,"ms"])$y } else { altitude=rep(NA,nrow(dum1))}
if(sum(! is.na(dum2[,"temperature"]))>2) { temperature = approx(x=dum2[,"ms"],y=dum2[,"temperature"], xout=dum1[,"ms"])$y } else { temperature=rep(NA,nrow(dum1))}
if(sum(! is.na(dum2[,"pressure"]))>2) { pressure = approx(x=dum2[,"ms"],y=dum2[,"pressure"], xout=dum1[,"ms"])$y } else { pressure=rep(NA,nrow(dum1))}
   my.data.frame = data.frame( dum1, latitude, longitude, altitude, temperature, pressure)
  #colnames(my.data.frame) = c(colnames(dum1)[1],"date_code",colnames(dum1)[2:ncol(dum1)], "latitude","longitude","altitude","temperature","pressure")
   invisible( my.data.frame)
	}
  	
    	
    	
