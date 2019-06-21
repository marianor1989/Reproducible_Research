``` r
setwd("C:/Users/U6065449/Downloads/nuevo")

activityDT <- data.table::fread(input = "./activity.csv")

Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = TRUE)), .SDcols = c("steps"), by = .(date)] 

head(Total_Steps,10)
```

    ##           date steps
    ##  1: 2012-10-01 10656
    ##  2: 2012-10-02   126
    ##  3: 2012-10-03 11352
    ##  4: 2012-10-04 12116
    ##  5: 2012-10-05 13294
    ##  6: 2012-10-06 15420
    ##  7: 2012-10-07 11015
    ##  8: 2012-10-08 10656
    ##  9: 2012-10-09 12811
    ## 10: 2012-10-10  9900

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.5.3

``` r
png("hist1.png",width = 480,height = 480)

ggplot(Total_Steps,aes(x=steps))+
  geom_histogram(fill="blue",binwidth = 1000)+
  labs(title = "daily steps",x="steps",y="Frecuency")

dev.off()
```

    ## png 
    ##   2

``` r
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

    ##    Mean_Steps Median_Steps
    ## 1:   10751.74        10656

``` r
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(IntervalDT, aes(x = interval , y = steps)) +
  geom_line(color="blue", size=1) +
  labs(title = "Avg. Daily Steps", x = "Interval", y = "Avg. Steps per day")
```

![](PA1_TEMPLATE_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
IntervalDT[steps == max(steps), .(max_interval = interval)]
```

    ##    max_interval
    ## 1:          835

``` r
activityDT[is.na(steps), .N ]
```

    ## [1] 0

``` r
nrow(activityDT[is.na(steps),])
```

    ## [1] 0

``` r
activityDT[is.na(steps), "steps"] <- round(activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps")])

data.table::fwrite(x = activityDT, file = "./activity.csv", quote = FALSE)



Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = TRUE)), .SDcols = c("steps"), by = .(date)] 

Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

    ##    Mean_Steps Median_Steps
    ## 1:   10751.74        10656

``` r
library(ggplot2)
ggplot(Total_Steps, aes(x = steps)) +
  geom_histogram(fill = "blue", binwidth = 1000) +
  labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_TEMPLATE_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
activityDT[, dateTime := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = dateTime)]

activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]

activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_TEMPLATE_files/figure-markdown_github/unnamed-chunk-1-3.png)
