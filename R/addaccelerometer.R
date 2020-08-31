#' Add accelerometer data to the AnitaAccelerometer collection in the PLMResearch database.
#'
#' This function adds accelerometer data from Anita's Belmont trial to the MongoDB database.
#' @name addaccelerometer
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date a list of cattle dates
#' @param hour a list of cattle hours
#' @param roundedtime a list of cattle rounded times
#' @param timestamp a list of cattle timestamps
#' @param X the x axes
#' @param Y the Y axes
#' @param Z the Z axes
#' @param status the status
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import dplyr
#' @import mongolite
#' @export


addaccelerometer <- function(RFID, mtag, date, hour, roundedtime, timestamp, X, Y, Z, status, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  acc <- mongo(collection = "AnitaAccelerometer", db = "PLMResearch", url = pass, verbose = T)

  df <- data.frame(RFID, mtag, date, hour, roundedtime, timestamp, X, Y, Z, status)
  time <- data.frame(time = unique(timestamp), stringsAsFactors = FALSE)
  template <- acc$find(query = sprintf('{"RFID":"xxxxxx"}'), fields = '{"_id":false}')

  for(i in 1:nrow(time)){
    dt <- df %>%
      filter(timestamp == time$time[i])

    template$RFID <- dt$RFID[1]
    template$management <- as.character(dt$mtag[1])
    template$date <- dt$date[1]
    if(nchar(as.character(dt$timestamp[1]))==10){
      template$sort <- as.character(paste(dt$timestamp[1], "00:00:00"))
    } else {
      template$sort <- as.character(dt$timestamp[1])
    }
    template$status <- dt$status[1]

    acc$insert(template)

    for(p in 1:nrow(dt)){
      match <- sprintf('{"RFID":"%s", "sort":"%s"}', template$RFID, template$sort)

      if(nchar(as.character(dt$hour[1] == 10))){
        dt$hour <- paste(dt$hour[1], "00:00:00")
      }
      if(nchar(as.character(dt$roundedtime[1] == 10))){
        dt$roundedtime <- paste(dt$roundedtime[1], "00:00:00")
      }
      if(nchar(as.character(dt$timestamp[1] == 10))){
        dt$timestamp <- paste(dt$timestamp[1], "00:00:00")
      }

      accelerometerdata <- sprintf('{"$set":{"hour":{"$date":"%s"}, "roundedtime":{"$date":"%s"}, "timestamp":{"$date":"%s"},
                                   "X.%s":%s, "Y.%s":%s, "Z.%s":%s}}',
                                   paste0(substr(dt$hour[1],1,10),"T",substr(dt$hour[1],12,19),"+1000"),
                                   paste0(substr(dt$roundedtime[1],1,10),"T",substr(dt$roundedtime[1],12,19),"+1000"),
                                   paste0(substr(dt$timestamp[1],1,10),"T",substr(dt$timestamp[1],12,19),"+1000"),
                                   p-1, dt$X[p], p-1, dt$Y[p], p-1, dt$Z[p])

      acc$update(match, accelerometerdata)
    }
  }
}
