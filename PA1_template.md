dataset <- read.csv("activity.csv")

tidyday <- aggregate(steps ~ date, data = dataset, FUN = sum, na.rm = TRUE)

hist(tidyday$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "wheat")


meansteps <- mean(tidyday$steps)
mediansteps <- median(tidyday$steps)

meansteps <- format(meansteps,digits=1)
mediansteps <- format(mediansteps,digits=1)


tidydaymean <- aggregate(steps ~ interval, data = dataset, FUN = mean, na.rm = TRUE)

plot(tidydaymean$interval, tidydaymean$steps, type = "l", col = "tan3", xlab = "Intervals", ylab = "Total steps per interval", main = "Number of steps per interval (averaged) (NA removed)")



maxsteps <-max(tidydaymean$steps)


maxinterval <- tidydaymean$interval[which(tidydaymean$steps == maxsteps)]
maxsteps <- round(maxsteps, digits = 2)


sum(is.na(dataset))



mvals <- subset(dataset, is.na(steps))




MeanStepsPerInterval <- tapply(dataset$steps, dataset$interval, mean, na.rm = TRUE)


tidynon <- dataset[is.na(dataset$steps),]
tidynonnon <- dataset[!is.na(dataset$steps),]


tidynon$steps <- as.factor(tidynon$interval)
levels(tidynon$steps) <- MeanStepsPerInterval


levels(tidynon$steps) <- round(as.numeric(levels(tidynon$steps)))
tidynon$steps <- as.integer(as.vector(tidynon$steps))


cutdata <- rbind(tidynon, tidynonnon)


par(mfrow = c(1,2))


tidyday <- aggregate(steps ~ date, data = dataset, FUN = sum, na.rm = TRUE)
hist(tidyday$steps, xlab = "Steps per Day", main = "NAs REMOVED - Total steps/day", col = "wheat")


cuttidyday <- aggregate(steps ~ date, data = cutdata, FUN = sum, na.rm = TRUE)
hist(cuttidyday$steps, xlab = "Steps per Day", main = "NAs IMPUTED - Total steps/day", col = "wheat")


cutmeansteps <- mean(cuttidyday$steps)
cutmediansteps <- median(cuttidyday$steps)


cutmeansteps <- format(cutmeansteps,digits=1)
cutmediansteps <- format(cutmediansteps,digits=1)


resultsmeanmedian <- data.frame(c(meansteps, mediansteps), c(cutmeansteps, cutmediansteps))
colnames(resultsmeanmedian) <- c("NA removed", "Imputed NA values")
rownames(resultsmeanmedian) <- c("mean", "median")


library(xtable)

xt <- xtable(resultsmeanmedian)
print(xt, type  = "html")




cutdata$dayType <- ifelse(weekdays(as.Date(cutdata$date)) == "Samstag" | weekdays(as.Date(cutdata$date)) == "Sonntag", "weekend", "weekday")


cutdata$dayType <- factor(cutdata$dayType)



stepsintervaldayType <- aggregate(steps ~ interval + dayType, data = cutdata, FUN = mean)


head(stepsintervaldayType)



names(stepsintervaldayType) <- c("interval", "daytype", "meansteps")


library(ggplot2)
plot <- ggplot(stepsintervaldayType, aes(interval, meansteps))
plot + geom_line(color = "tan3") + facet_grid(daytype~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")


