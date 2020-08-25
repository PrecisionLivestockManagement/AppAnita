#' Add cattle data to the AnitaCattle collection in the PLMResearch database.
#'
#' This function adds cattle data from Anita's Belmont trial to the MongoDB database.
#' @name add_cattle
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param calvingdate a list of cattle calving dates
#' @param paddock the paddock of the cow
#' @param date a list of dates
#' @param hour a list of cattle timestamps corresponding to the dates
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_cattle <- function(RFID, mtag, calvingdate, paddock, date, hour, status, username = username, password = password){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "AnitaCattle", db = "PLMResearch", url = pass, verbose = T)

  cattledata <- sprintf(
    '{"RFID":"%s", "Management":"%s", "calvingdate":"%s", "paddock":"%s", "date":"%s", "hour":{"$date":"%s"}, "status":"%s"}',
    RFID, mtag, calvingdate, paddock, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), status)

  cattle$insert(cattledata)
}
