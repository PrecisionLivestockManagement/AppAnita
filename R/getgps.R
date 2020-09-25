#' Retrieves GPS data from the AnitaGPS collection in the PLMResearch database.
#'
#' This function retrieves GPS data from Anita's Belmont trial to the MongoDB database.
#' @name getgps
#' @param timestamp a list of timestamps
#' @param status a list of cattle statuses to search for
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return returns the cattle spatial information
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


getgps <- function(time = NULL, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPSer <- mongo(collection = "AnitaGPS", db = "PLMResearch", url = url, verbose = T)

  if(is.null(time)){
    info <- GPSer$find()
  } else {
    time <- paste(unlist(time), collapse = '", "')
    time <- sprintf('"time":{"$date":"%s"},', strftime(as.POSIXct(paste0(time)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

    filter <- paste0("{", time, "}")

    if(nchar(filter)==2){}else{
      filter <- substr(filter, 1 , nchar(filter)-2)
      filter <- paste0(filter, "}")}
    info <- GPSer$find(query = filter)
  }
}
