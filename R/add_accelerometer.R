#' Add accelerometer data to the AnitaAccelerometer collection in the PLMResearch database.
#'
#' This function adds accelerometer data from Anita's Belmont trial to the MongoDB database.
#' @name add_accelerometer
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_gps <- function(RFID, mtag, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  accelerometer <- mongo(collection = "AnitaAccelerometer", db = "PLMResearch", url = pass, verbose = T)

  accelerometerdata <- sprintf(
    '{"RFID":"%s", "Management":"%s", "calvingdate":"%s", "timestamp":{"$date":"%s"},
    "roundedtime":{"$date":"%s"},  "lat":%s, "long": %s ,"proximity":"%s", "neighbour":"%s", "paddock":"%s", "status":"%s"}',
    RFID, mtag, calvingdate, paste0(substr(timestamp,1,10),"T",substr(timestamp,12,19),"+1000"),
    paste0(substr(roundedtime,1,10),"T",substr(roundedtime,12,19),"+1000"), lat, long, proximity, neighbour, paddock, status)

  accelerometer$insert(accelerometerdata)
}
