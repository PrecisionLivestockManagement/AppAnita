#' Add weather data to the AnitaWeather collection in the PLMResearch database.
#'
#' This function adds weather data from Anita's Belmont trial to the MongoDB database.
#' @name addweather
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


addweather <- function(date, time, rain, temp, humidity, THI, condition, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  weath <- mongo(collection = "AnitaWeather", db = "PLMResearch", url = url, verbose = T)

  weatherdata <- sprintf(
    '{"date":"%s", "time":{"$date":"%s"}, "rain":%s, "temperature":%s, "humidity":%s, "THI":%s, "condition":"%s"}',
    date, paste0(substr(time,1,10),"T",substr(time,12,19),"+1000"), rain, temp, humidity, THI, condition)

  weath$insert(weatherdata)
}
