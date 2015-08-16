library(ggplot2)
library(knitr)
##
unzip("./activity.zip")
data <- read.csv("./activity.csv", header=T, na.strings="NA")
totalsteps_day <- aggregate(steps ~ date, data, FUN = sum, simplify = T, na.rm =T)
Avg_steps_day <- aggregate(steps ~ date, data, FUN = mean, simplify = T, na.rm =T)
qplot(totalsteps_day$steps, geom = "histogram", xlab = "Total steps per day", binwidth=500)
summary(totalsteps_day$steps)
avgSteps <- mean(totalsteps_day$steps, na.rm =F)
medSteps <- median(totalsteps_day$steps, na.rm=F)
##
avgSteps_TimeBlock <- aggregate(x=list(avgSteps=data$steps), by=list(interval=data$interval), FUN=mean, na.rm=TRUE)
png("./figure/plot1.png")
p1 <- qplot(avgSteps_TimeBlock$interval, avgSteps_TimeBlock$avgSteps, geom="line", type="l") 
p1 <- p1 + xlab("5 minute Time interval") + ylab("Average steps taken")
print(p1)
dev.off()
##
maxSteps_TimeBlock <- which.max(avgSteps_TimeBlock$avgSteps)
Time_maxSteps <-  gsub("([0-9]{1,2})([0-9]{2})", "\\1:\\2", avgSteps_TimeBlock[maxSteps_TimeBlock,'interval'])
##
numNA <- length(which(is.na(data$steps)))
##
data_Impute = data
data_Impute$steps[is.na(data$steps)] = mean(data$steps, na.rm=T)

#class(data_Imputed$steps) <- "integer"
totalsteps_dayAfterImpute <- aggregate(steps ~ date, data_Imputed, FUN = sum, simplify = T)
qplot(totalsteps_dayAfterImpute$steps, geom = "histogram", xlab = "Total steps per day", binwidth=1000)
summary(totalsteps_dayAfterImpute$steps)
avgStepsAfterImpute <- mean(totalsteps_dayAfterImpute$steps)
medStepsAfterImpute <- median(totalsteps_dayAfterImpute$steps)

##
data_Imputed$wday <-  ifelse(as.POSIXlt(data_Imputed$date)$wday %in% c(0,6), 'weekend', 'weekday')
avgSteps_wday <- aggregate(steps ~ interval + wday, data_Imputed, FUN = mean, simplify = T)
png("./figure/plot2.png")
p2 <- ggplot(avgSteps_wday, aes(interval,steps)) + geom_line() + facet_grid(wday ~ .)
p2 <- p2 +  xlab("5 minute Time interval") + ylab("Average steps taken")
print(p2)
dev.off()


