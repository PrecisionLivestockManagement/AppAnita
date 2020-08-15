#' Add GPS data
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name add_weather
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param calvingdate a list of cattle calving dates
#' @param timestamp a list of cattle timestamp of a coordinate point
#' @param roundedtime a list of cattle timestamps, rounded to the closest 5 minutes
#' @param lat the latitude of a coordinate point
#' @param long the longitude of a coordinate point
#' @param proximity the proximity to the closest neighbour
#' @param neighbour the closest neighbouring cow
#' @param paddock the paddock that the cattle are in
#' @param status the calving status of the cow
#' @param username if you don't have a username set up using the dmaccess function you can pass a username, if no value added then the function looks for a value from dmaccess via keyring
#' @param password if you include a username you will also need to add a password contact Lauren O'Connor if you don't have access
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
