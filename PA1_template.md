# Reproducible Research: Peer Assessment 1



```r
library(dplyr, quietly = TRUE)
library(data.table, quietly = TRUE)
library(lattice, quietly = TRUE)
```

## Loading and preprocessing the data


```r
fpath <- "activity.csv"

if (!file.exists(fpath)) {
        if (!file.exists("activity.zip")) {
        library(downloader, quietly = TRUE)
        download(url, "activity.zip")
        }
        unzip("activity.zip", files = "activity.csv")
}
df <- fread(fpath)
```
  
  
###2. Transforming the data  


```r
df <- df %>%
        tbl_df() %>%
        mutate(date = as.Date(date))
```

## What is mean total number of steps taken per day?

Note: For this part of the assignment, you can ignore the missing values in the dataset.

###1. Calculating the total number of steps taken per day  


```r
daily <- df %>%
         filter(!is.na(steps)) %>%
         group_by(date) %>%
         summarise(steps = sum(steps))
```

###2. Histogram of the total number of steps taken each day  


```r
hist(daily$steps, col = "green", main = "Histogram of Daily Steps",
xlab = "Daily Steps")
```

![](PA1_template_files/figure-html/hist_daily-1.png) 

###3. Calculating and reporting the mean and median of the total number of steps taken per day  


```r
mean_daily <- mean(daily$steps, na.rm = TRUE)
median_daily <- median(daily$steps, na.rm = TRUE)
```
####Answer: 
####The mean total number of daily steps is **10766**. The median of the total number of daily steps is **10765**.  


## What is the average daily activity pattern?

###1. Making a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.  


```r
ts <- df %>% 
        group_by(interval) %>% 
        summarise(steps = mean(steps, na.rm = TRUE))
```


```r
with(ts, 
     plot(interval, steps, type = "l", lwd = 1, col = "blue", 
          main = "Time Series of Average Steps", 
          xlab = "Time Series (5-minute interval)", 
          ylab = "Number of steps")
     )
```

![](PA1_template_files/figure-html/ts_plot-1.png) 
    
###2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
max_ind <- which(ts$steps == max(ts$steps, na.rm = TRUE))
interval_max <- ts$interval[max_ind]
```
  
####Answer: The 5-minute interval **835** contains the maximum number of steps, on average across all the days in the dataset.    
   

## Imputing missing values

###1. Calculating and reporting the total number of missing values in the dataset (i.e. the total number of rows with NAs)  


```r
n_NA <- df %>%
        filter(is.na(steps)) %>%
        nrow()
```

#### Answer: The total number of missing values in the dataset is **2304**.  
  
  
###2. Devising a strategy for filling in all of the missing values in the dataset.   
####Strategy of filling: using the mean for that 5-minute interval  


```r
ind_NA <- which(is.na(df$steps))
interval_NA <- df$interval[ind_NA]
```
   
###3. Creating a new dataset that is equal to the original dataset but with the missing data filled in.  


```r
fill <- df
for (i in ind_NA) {
        fill$steps[i] <- ts$steps[which(ts$interval == df$interval[i])]
}  
```
  
###4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

####    1). Making histogram of the total number of steps taken each day.  


```r
daily_fill <- fill %>%
                group_by(date) %>%
                summarise(steps = sum(steps))
```

```r
hist(daily_fill$steps, col = "blue", main = "Histogram of Daily Steps (imputed)",
xlab = "Daily Steps")
```

![](PA1_template_files/figure-html/hist_daily_fill-1.png) 

####    2). Calculating and reporting the mean and median total number of steps taken per day  


```r
mean_daily_fill <- mean(daily_fill$steps, na.rm = T)
median_daily_fill <- median(daily_fill$steps, na.rm = T)
```

####Answer: The mean and median of the total number of daily steps are 
**10766** and **10766**.  
  
####    3). Do these values differ from the estimates from the first part of the assignment? 

```r
identical(mean_daily, mean_daily_fill)
```

```
## [1] TRUE
```

```r
all.equal(median_daily, median_daily_fill, tolerance = 0.0002)
```

```
## [1] TRUE
```
  
####Answer: The mean and median values of the total number of steps taken per day are same before and after imputing for the missing data.  
  
####    4). What is the impact of imputing missing data on the estimates of the total daily number of steps?  
  
####Answer: The impact of imputing missing data on the estimates of the total daily number of steps is that it increases the frequencies, here the number of observations or number of days for some range of the average daily steps. The following side by side 2-panel histograms show that the number of days for the daily steps between 10000 and 15000 is increased significantly.  


```r
par(mfrow = c(1,2))
 
hist(daily$steps, ylim = c(0, 35), col = "green", main = "Histogram of Daily Steps", xlab = "Daily Steps")
abline(v = median_daily, lwd = 2, col = "wheat")

hist(daily_fill$steps, ylim = c(0, 35), col = "blue", main = "Histogram of Daily Steps (filled-in)", xlab = "Daily Steps")
abline(v = median_daily, lwd = 2, col = "wheat")
```

![](PA1_template_files/figure-html/hist_compare-1.png) 
   

## Are there differences in activity patterns between weekdays and weekends?
###1. Creating a new factor variable in the dataset with two levels â âweekdayâ and âweekendâ indicating whether a given date is a weekday or weekend day.  


```r
weekde <- c(1:nrow(df))
weekde[] <- "weekday"

ind1 <- which(weekdays(fill$date, abbreviate = TRUE) %in% c("Sat", "Sun"))
weekde[ind1] <- "weekend"

fill <- mutate(fill, weekde = factor(weekde))
```
   
###2. Making a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.  


```r
ts_fill <- fill %>%
                group_by(interval, weekde) %>%
                summarise(steps = mean(steps, na.rm = T))
```

```r
xyplot(steps ~ interval | weekde, data = ts_fill, type = "l", col = "blue", layout = c(1, 2), main = "Time series of steps", xlab = "Time series (5-minute interval)")
```

![](PA1_template_files/figure-html/ts_wkdays-1.png) 

####As the above time series plot shows that there are differences in activity patterns between weekdays and weekends.  
##SessionInfo

```r
sessionInfo()
```

```
## R version 3.2.2 (2015-08-14)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United States.1252 
## [2] LC_CTYPE=English_United States.1252   
## [3] LC_MONETARY=English_United States.1252
## [4] LC_NUMERIC=C                          
## [5] LC_TIME=English_United States.1252    
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] lattice_0.20-33  data.table_1.9.4 dplyr_0.4.2     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.12.0     digest_0.6.8    assertthat_0.1  grid_3.2.2     
##  [5] chron_2.3-47    plyr_1.8.3      R6_2.1.0        DBI_0.3.1      
##  [9] formatR_1.2     magrittr_1.5    evaluate_0.7.2  stringi_0.5-5  
## [13] lazyeval_0.1.10 reshape2_1.4.1  rmarkdown_0.7   tools_3.2.2    
## [17] stringr_1.0.0   yaml_2.1.13     parallel_3.2.2  htmltools_0.2.6
## [21] knitr_1.11
```

