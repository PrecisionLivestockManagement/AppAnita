#' Add accelerometer data to the AnitaAccelerometer collection in the PLMResearch database.
#'
#' This function adds accelerometer data from Anita's Belmont trial to the MongoDB database.
#' @name getcalvingmodel
#' @param hour
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import caret
#' @export


getcalvingmodel <- function(hour, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  calvmod <- mongo(collection = "AnitaCalvingModelData", db = "PLMResearch", url = pass, verbose = T)

  hour <- "2018-10-16 13:00:00"
  hour <- paste(unlist(hour), collapse = '", "')
  hour <- sprintf('"hour":{"$date":"%s"},', strftime(as.POSIXct(paste0(hour)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
  hour <- paste0("{", hour ,"}")

  if(nchar(hour)==2){}else{
    hour <- substr(hour, 1 , nchar(hour)-2)
    hour <- paste0(hour, "}")}

  testing <- calvmod$find(query = hour)
}
