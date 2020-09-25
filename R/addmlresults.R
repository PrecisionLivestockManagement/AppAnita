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


addgps <- function(RFID, mtag, date, time, svm, cart, calvingdate, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  mlres <- mongo(collection = "AnitaMLResults", db = "PLMResearch", url = url, verbose = T)

  mlresdata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "time":{"$date":"%s"},
    "svm":"%s", "cart":"%s", "calvingdate":"%s"}',
    RFID, mtag, date, paste0(substr(time,1,10),"T",substr(time,12,19),"+1000"),
    svm, cart, calvingdate)

  mlres$insert(mlresdata)
}
