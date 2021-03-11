#' Clean Raw Sensor Data
#'
#' This function cleans up the raw readings from the PA sensors and sets up a data frame.
#' It assumes that the data has the columns
#' c('timestamp', "channelAPm25", "channelBPm25", "humidity", "latitude", "longitude") in that order.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Any readings with faulty channel A/B values will be dropped
#'
#' @param data a .csv of PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the full PurpleAir sensor data, with corrected timezone and combined channels
#' @export

cleanPA <- function(data){
  time_clean = lubridate::ymd_hms(data$timestamp, tz="America/Los_Angeles")

  data$timestamp <- time_clean
  data <- data[order(data$timestamp),]
  data[,5] <- round(data[,5],5)
  data[,6] <- round(data[,6],4)

  #throwing out unusable data (by EPA standard)
  data$avgPM <- (data$channelAPm25+ data$channelBPm25)/2
  data$difference <- abs(data$channelAPm25- data$channelBPm25)
  data$pct_difference <- data$difference/ data$avgPM

  percent_threshold <- 0.7
  raw_threshold <- 5

  data <- dplyr::filter(data, difference <= raw_threshold & pct_difference <= percent_threshold)

  #EPA correction method for PurpleAir sensors
  data$PM2.5 <- 0.534 * data$avgPM - 0.0844 * data$humidity + 5.604
  data <- data[,c(1,10,4:6)]

  data
}



#' Collect Highs and Lows for PM2.5 per day
#'
#' This function collects the daily high and low values of PM2.5 for each sensor into a dataframe.
#' It assumes that the data has the columns 'timestamp' and 'longitude'.
#' It also uses an EPA correction factor for PurpleAir sensors.
#' Anyidentical readings of duplicate type will be dropped
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export

highslows <- function(data) {

  sensors <- unique(data[,c('longitude','latitude')])

  sensorNum <- nrow(sensors)

  hi_lo <- data.frame()

  for (i in 1:sensorNum) {
    high <- aggregate(cbind(PM2.5, latitude, longitude) ~ day,
                      data = data[data$longitude == sensors[i,1] & lubridate::month(data$timestamp) >1,], max)

    nhighs <- nrow(high)
    type <- c(type = rep(c("high"), nhighs))
    high$type <- type

    low <- aggregate(cbind(PM2.5, latitude, longitude) ~ day,
                     data = data[data$longitude == sensors[i,1] & lubridate::month(data$timestamp)>1, ], min)

    nvals <- nrow(low)
    type <- c(type = rep(c("low"), nvals))
    low$type <- type

    hi_lo <- rbind(hi_lo,high,low)
  }

  hi_lo <- dplyr::left_join(hi_lo, data, by = c("PM2.5", "day","latitude","longitude"), keep= F)
  hi_lo <- hi_lo[order(hi_lo$day),]

  hi_lo <- hi_lo[!duplicated(hi_lo[,c(1,2,5,11)]),]

  hi_lo
}







#' Down Sensors?
#'
#' This reports which sensors were down and on which days that they went
#'
#' @param data a dataframe of hourly PurpleAir sensor data from the CEHAT website
#' @return a dataframe of the highs and lows of PurpleAir sensor data, organized by sensor
#' @export
