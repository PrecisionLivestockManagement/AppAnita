#' Add walk over weigh data to the AnitaWoW collection in the PLMResearch database.
#'
#' This function adds weather data from Anita's Belmont trial to the MongoDB database.
#' @name addwow
#' @param timestamp a list of timestamps for weather
#' @param rain the precipitation
#' @param temp the temperature (T)
#' @param humidity the humidity (H)
#' @param THI the temperature-humidity index. Calculated as: 0.8×T+H×(T-14.4)+46.4
#' @param condition the weather conditions
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addwow <- function(RFID, mtag, date, time, timestamp, weight, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  wower <- mongo(collection = "AnitaWoW", db = "PLMResearch", url = url, verbose = T)

  wowdata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "time":{"$date":"%s"}, "timestamp":{"$date":"%s"},
    "weight":%s}',
    RFID, mtag, date, paste0(substr(time,1,10),"T",substr(time,12,19),"+1000"), paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"),
    weight)

  wower$insert(wowdata)
}
