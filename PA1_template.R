### Reproducible Research: Peer Assessment 1


### Loading and preprocessing the data --------------------------------------------------------

# 1. Load the data (i.e. read.csv())
#     Note: assumed that the user and the data are in the working directory.

      dframe <- read.csv("activity.csv", na.strings = "NA")


# 2. Process/transform the data (if necessary) into a format suitable for your analysis

      dframe$date <- as.POSIXct(dframe$date, format="%Y-%m-%d")         # Convert the data field to a date format.
      dframe$daytype <- as.factor(tolower(weekdays(dframe$date)))       # Add a column to the data frame for the day of the week.

      
      # label the days as weekend or weekday
            daylabeler <- function(daytype, data = dframe){
                  daylabel=ifelse(dframe$daytype == "saturday" | dframe$daytype == "sunday", "weekend", 
                           "weekday")
            }

      dframe$daylabel <- as.factor(daylabeler(dframe$daytype))          # Run the function and add a column to the data frame for a weekend or weekday label.
      rm(daylabeler)                                                    # Clean the workspace.

      
      
      
###   What is mean total number of steps taken per day?   ---------------------------------------

# 1. Calculate total steps per day.
      
      daySteps <- aggregate(x=list(steps=dframe$steps), by = list(dframe$date), FUN = sum, na.rm=TRUE)      

      
# 2. Make a histogram of the total number of steps taken each day
      
      hist(daySteps$steps, xlab = "Steps", main = "Total Steps per Day", breaks = 50, col = "blue")


# 3. Calculate the Mean and Median steps taken per day
      
      meanSteps <- mean(daySteps$steps, na.rm = TRUE)             # Mean steps per day.
      
            # The mean is 9354.23 steps per day.
      
      medianSteps <- median(daySteps$steps, na.rm = TRUE)         # Median steps per day.
      
            # The median is 10,395 steps per day.
      
      rm(daySteps)                                                # Clean the workspace.



###  What is the average daily activity pattern? ---------------------------------------------

# 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
      
      # create an averages by interval variable
            
            averages <- aggregate(x=list(steps=dframe$steps), by=list(interval=dframe$interval), FUN=mean, na.rm=TRUE) 
      
      # plot the averages variable
            
            plot(averages$interval, averages$steps, type = "l", xlab = "Daily 5-min Intervals", ylab = "Steps")



# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
      
      averages[which.max(averages$steps),]

            # interval 104 at 8:35 AM has the highest average number of steps at 206.17 steps.
      
      rm(averages)                                                # Clean the workspace.


      
      
      

# Imputing missing values   --------------------------------------------------------------------------

# 1. Calculate and report the total number of missing values in the dataset.
      
      misVals <- which(is.na(dframe$steps))     # a vector of the index location of missing values
      length(misVals)
      
            # There are 2304 missing values in the dataset.


# 2. Devise a strategy for filling in all of the missing values in the dataset.
      
      # Fill the missing values with the mean.
            # Calculate the mean value of steps per interval.
            # Store this mean value in a vector equal in length to the vector of missing values.
            # Copy the data frame.
            # Replace the missing step values with the mean values by index location.

      
# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

      means <- rep(mean(dframe$steps, na.rm=TRUE), times=length(misVals))     # a vector of means equal in length to the missing values vecor
      newframe <- dframe                                                      # new copy of the original data frame
      newframe[misVals,"steps"] <- means                                      # replace the values of the missing value locations to equivalent value in the mean vector
      rm(means, misVals)                                                      # Clean the workspace.

      
# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
      # Do these values differ from the estimates from the first part of the assignment?
      # What is the impact of imputing missing data on the estimates of the total daily number of steps?
      
      # Make histogram
      
      daySteps2 <- aggregate(x=list(steps=newframe$steps), by = list(newframe$date), FUN = sum, na.rm=TRUE)
      hist(daySteps2$steps, xlab = "Steps", main = "Total Steps per Day", breaks=seq(from=0, to=25000, by=2500), col = "blue")


      # Calculate new mean and median values for the data now that missing values have been imputed

      meanSteps2 <- mean(daySteps2$steps, na.rm = TRUE)
      
            # The new mean is 10,766.19 steps per day.
      
      medianSteps2 <- median(daySteps2$steps, na.rm = TRUE)
      
            # The new median is 10,766.19 steps per day.

      
      
      # Do these values differ from the estimates from the first part of the assignment?
            
            meanSteps2 - meanSteps
            
            # The means differ by 1411.96 steps. The mean has gone up now that missing values have been imputed.
            
            medianSteps2 - medianSteps

            # The medians differ by 371.19 steps. The median has gone up now that missing values have been imputed.
      
                  
      
      # What is the impact of imputing missing data on the estimates of the total daily number of steps?      
      
            # Imputing values for the missing data has increased the mean by 1411.96 steps and the median by 371.19 steps.
            
            

            
### Are there differences in activity patterns between weekdays and weekends?  ------------------------------------------------------------
      
# 1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
            # This part of the assignment was performed in part 2 of the loading and processing data step at the beginning.
            

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken,
            # averaged across all weekday days or weekend days (y-axis). 

            averages2 <- aggregate(steps ~ interval + daylabel, data = newframe, FUN=mean, na.rm=TRUE)
            library(lattice)
            xyplot(steps ~ interval | daylabel, averages2, layout = c(1,2), type = "l") 

            # Weekends appear to have a higher average of steps per day.


