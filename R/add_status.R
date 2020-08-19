#' Add status data to the AnitaStatus collection in the PLMResearch database.
#'
#' This function adds status data from Anita's Belmont trial to the MongoDB database.
#' @name add_status
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param calvingdate a list of cattle calving dates
#' @param timestamp a list of cattle timestamps of a coordinate point
#' @param roundedtime a list of cattle timestamps, rounded to the closest 5 minutes
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_status <- function(RFID, mtag, calvingdate, timestamp, roundedtime, status, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  stat <- mongo(collection = "AnitaStatus", db = "PLMResearch", url = pass, verbose = T)

  statdata <- sprintf(
    '{"RFID":"%s", "Management":"%s", "calvingdate":"%s", "timestamp":{"$date":"%s"},
    "roundedtime":{"$date":"%s"}, "status":"%s"}',
    RFID, mtag, calvingdate, paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"),
    paste0(substr(roundedtime,1,10),"T",substr(roundedtime,12,19),"+1000"), status)

  stat$insert(statdata)
}
