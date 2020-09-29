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

  filter <- sprintf('{"RFID":"%s", "date":"%s"}', RFID, date)

  data <- stat$find(query = filter)

  for (i in 1:nrow(data)){
    RFIDS <- sprintf('{"RFID":"%s", "time":{"$date":"%s"}}', data$RFID[i], strftime(as.POSIXct(paste0(data$time[i])), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    IDI <- sprintf('{"$set":{"incorrectalert":"%s"}}', "pregnant")
    stat$update(RFIDS, IDI)
  }
}
