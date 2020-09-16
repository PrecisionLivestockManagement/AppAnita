#' Retrieves weather data from the AnitaWeather collection in the PLMResearch database.
#'
#' This function retrieves weather data from Anita's Belmont trial to the MongoDB database.
#' @name updatestatus
#' @param timestamp a list of timestamps
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return returns the paddock spatial information
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


updatestatus <- function(RFID, status, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  stat <- mongo(collection = "AnitaStatus", db = "PLMResearch", url = pass, verbose = T)

  RFID <- paste(unlist(RFID), collapse = '", "')
  RFID <- sprintf('"RFID":{"$in":["%s"]},', RFID)
  RFID <- paste0("{", RFID, "}")

  if(nchar(RFID)==2){}else{
    RFID <- substr(RFID, 1 , nchar(RFID)-2)
    RFID <- paste0(RFID, "}")}

  info <- stat$find(query = RFID)

  RFIDS <- sprintf('{"RFID":"%s"}', info$RFID)
  inf <- sprintf('{"$set":{"status":"%s"}}', status)

  stat$update(RFIDS, inf)
}