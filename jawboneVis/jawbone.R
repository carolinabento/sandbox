setwd("path-to-work-dir")

#package to convert r objects to json
library("rjson")
#package for improved data manipulation
library("gdata")

#load the data
data <- read.csv("path-to-data")
#remove age,gender,goal_body_weight,goal_body_weight_intent,height,weight columns
cleanData <- data[,-c(2,32,33,34,35,70)]
data <- cleanData
#convert measured active time to hours
#m_active_time::: total number of hours the user has been moving
data$m_active_time <- (data$m_active_time)/3600

#convert distance to kms
#m_distance::: total distance in meters
data$m_distance <- (data$m_distance)/1000

#data frame with date|distance
data <- na.omit(data.frame(date=data$DATE,distance=data$m_distance),quote=FALSE)

data$utcString <- as.vector(paste(paste(paste("[",as.numeric(as.POSIXct(dayDistance[,1], format="%Y%m%d")),sep=""),dayDistance$distance,sep=","),"]",sep=""))

dataFrame <- data.frame(data,stringsAsFactors=FALSE)

#JSON Object
jsonDayDistance <- toJSON(as.list(dataFrame))

#output file for daily walked distance
write.table(jsonDayDistance,file="dayDistance.json",quote=FALSE,row.names=FALSE, col.names=FALSE)


#function to get average distance by month
distanceByMonth <- function(){
	augDistance <- vector()
	sepDistance <- vector()
	octDistance <- vector()
	novDistance <- vector()
	decDistance <- vector()

	i <- 1
	while(i < nrow(data)) {
		
		if(startsWith(data[i,1], '201408'))
			augDistance <- c(augDistance, data[i,2])

		if(startsWith(data[i,1], '201409'))
			sepDistance <- c(sepDistance, data[i,2])

		if(startsWith(data[i,1], '201410'))
			octDistance <- c(octDistance, data[i,2])

		if(startsWith(data[i,1], '201411'))
			novDistance <- c(novDistance, data[i,2])

		if(startsWith(data[i,1], '201412'))
			decDistance <- c(decDistance, data[i,2])

		i <- i + 1
	}


	aug <- round(sum(augDistance)/length(augDistance),2)
	sep <- round(sum(sepDistance)/length(sepDistance),2)
	oct <- round(sum(octDistance)/length(octDistance),2)
	nov <- round(sum(novDistance)/length(novDistance),2)
	dec <- round(sum(decDistance)/length(decDistance),2)

	months <- c("Aug","Sep","Oct", "Nov","Dec")
	averages <- data.frame(names=months,averages=c(aug,sep,oct,nov,dec))

	jsonMonthData <- toJSON(c(as.list(averages)))

    #output file for average walked distance by month
	write.table(jsonMonthData,file="monthDistance.json",quote=FALSE,row.names=FALSE, col.names=FALSE)
}

distanceByMonth()


