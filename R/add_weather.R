#' Add weather data to the AnitaWeather collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name add_weather
#' @param timestamp a list of timestamps for weather
#' @param rain the precipitation
#' @param temp the temperature (T)
#' @param humidity the humidity (H)
#' @param THI the temperature-humidity index. Calculated as: 0.8×T+H×(T-14.4)+46.4
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_weather <- function(timestamp, rain, temp, humidity, THI, username, password){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  weather <- mongo(collection = "AnitaWeather", db = "PLMResearch", url = pass, verbose = T)

  weatherdata <- sprintf(
    '{"timestamp":{"$date":"%s"}, "rain":%s, "temperature":%s, "humidity":%s, "THI":%s}',
    paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"), rain, temp, humidity, THI)

  weather$insert(weatherdata)
}
