#' Retrieves weather data from the AnitaWeather collection in the PLMResearch database.
#'
#' This function retrieves weather data from Anita's Belmont trial to the MongoDB database.
#' @name updateincorrectalert
#' @param status the status
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return returns the paddock spatial information
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


updateincorrectalert <- function(RFID, date, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  stat <- mongo(collection = "AnitaMLResults", db = "PLMResearch", url = url, verbose = T)

  for (i in 1:length(RFID)){
    RFIDS <- sprintf('{"RFID":"%s", "date":"%s"}', RFID[i], date[i])
    IDI <- sprintf('{"$set":{"incorrectalert":"%s"}}', "pregnant")
    stat$update(RFIDS, IDI)
  }
}
