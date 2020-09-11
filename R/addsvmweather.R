#' Add GPS data to the AnitaGPS collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name addgps
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date the date of the coordinate point
#' @param hour the hour of the coordinate point
#' @param period the period
#' @param hour.rain
#' @param hour.humidity
#' @param hour.THI
#' @param day.minrain
#' @param day.rangerain
#' @param day.maxhumidity
#' @param day.meanhumidity
#' @param day.minTHI
#' @param day.meanTHI
#' @param day.sdTHI
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addgps <- function(RFID, mtag, date, hour, period, hour.rain, hour.humidity, hour.THI, day.minrain, day.rangerain,
                   day.maxhumidity, day.meanhumidity, day.minTHI, day.meanTHI, day.sdTHI, status, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  calvmod <- mongo(collection = "AnitaSVMWeather", db = "PLMResearch", url = pass, verbose = T)

  calvmoddata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "hour":{"$date":"%s"}, "period":"%s", "hourrain":%s,
    "hourhumidity":%s, "hourTHI":%s, "dayminrain":%s, "dayrangerain":%s, "daymaxhumidity":%s, "daymeanhumidity":%s, "dayminTHI":%s,
    "daymeanTHI":%s, "daysdTHI":%s, "status":"%s"}',
    RFID, mtag, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), period, hour.rain, hour.humidity, hour.THI, day.minrain,
    day.rangerain, day.maxhumidity, day.meanhumidity, day.minTHI, day.meanTHI, day.sdTHI, status)

  calvmod$insert(calvmoddata)
}
