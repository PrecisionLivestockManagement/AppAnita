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


getgps <- function(timestamp = NULL, status = NULL, username = NULL, password = NULL){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPS <- mongo(collection = "AnitaGPS", db = "PLMResearch", url = pass, verbose = T)

  if(is.null(status)){status = c("preg", "calving", "dystocia", "with calf", "dead calf")}
  status <- paste(unlist(status), collapse = '", "')
  status <- sprintf('"status":{"$in":["%s"]},', status)

  if(is.null(timestamp)){
    filter <- paste0("{", status ,"}")
  } else {
    roundedtime <- paste(unlist(timestamp), collapse = '", "')
    roundedtime <- sprintf('"roundedtime":{"$date":"%s"},', strftime(as.POSIXct(paste0(roundedtime)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

    filter <- paste0("{", roundedtime, status ,"}")
  }

  if(nchar(filter)==2){}else{
    filter <- substr(filter, 1 , nchar(filter)-2)
    filter <- paste0(filter, "}")}
  fields <- sprintf('{"timestamp":true, "roundedtime":true, "status":true, "RFID":true, "Management":true, "lat":true, "long":true, "paddock":true, "_id":false}')
  info <- GPS$find(query = filter, fields = fields)

  return(info)

}