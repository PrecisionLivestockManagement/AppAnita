#' Add GPS data to the AnitaGPS collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name add_gps
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param calvingdate a list of cattle calving dates
#' @param timestamp a list of cattle timestamps of a coordinate point
#' @param roundedtime a list of cattle timestamps, rounded to the closest 5 minutes
#' @param lat the latitude of a coordinate point
#' @param long the longitude of a coordinate point
#' @param proximity the proximity to the closest neighbour
#' @param neighbour the closest neighbouring cow
#' @param paddock the paddock that the cattle are in
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_gps <- function(RFID, mtag, calvingdate, timestamp, roundedtime, lat, long, proximity, neighbour, paddock, status, username, password){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPS <- mongo(collection = "AnitaGPS", db = "PLMResearch", url = pass, verbose = T)

  GPSdata <- sprintf(
    '{"RFID":"%s", "Management":"%s", "calvingdate":"%s", "timestamp":{"$date":"%s"},
    "roundedtime":{"$date":"%s"},  "lat":%s, "long": %s ,"proximity":"%s", "neighbour":"%s", "paddock":"%s", "status":"%s"}',
    RFID, mtag, calvingdate, paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"),
    paste0(substr(roundedtime,1,10),"T",substr(roundedtime,12,19),"+1000"), lat, long, proximity, neighbour, paddock, status)

  GPS$insert(GPSdata)
}
