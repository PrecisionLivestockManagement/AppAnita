#' Add GPS data to the AnitaGPS collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name addgps
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date the date of the coordinate point
#' @param hour the hour of the coordinate point
#' @param roundedtime the timestamp of the coordinate point, rounded to the closest 5 minutes
#' @param timestamp the timestamp of the coordinate point
#' @param latitude the latitude of the coordinate point
#' @param longitude the longitude of the coorindate point
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addgps <- function(RFID, mtag, date, hour, roundedtime, timestamp, latitude, longitude, status, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPS <- mongo(collection = "AnitaGPS", db = "PLMResearch", url = pass, verbose = T)

  GPSdata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "hour":{"$date":"%s"}, "roundedtime":{"$date":"%s"}, "timestamp":{"$date":"%s"},
    "latitude":%s, "longitude": %s , "status":"%s"}',
    RFID, mtag, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), paste0(substr(roundedtime,1,10),"T",substr(roundedtime,12,19),"+1000"),
    paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"), latitude, longitude, status)

  GPS$insert(GPSdata)
}
