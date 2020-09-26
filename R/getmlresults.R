#' Add accelerometer data to the AnitaAccelerometer collection in the PLMResearch database.
#'
#' This function adds accelerometer data from Anita's Belmont trial to the MongoDB database.
#' @name getmlresults
#' @param time the time
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import caret
#' @export


getmlresults <- function(time = NULL, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  results <- mongo(collection = "AnitaMLResults", db = "PLMResearch", url = url, verbose = T)

  if(!is.null(time)){
    time <- paste(unlist(time), collapse = '", "')
    time <- sprintf('"time":{"$date":"%s"},', strftime(as.POSIXct(paste0(time)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
    time <- paste0("{", time ,"}")
    if(nchar(time)==2){}else{
      time <- substr(time, 1 , nchar(time)-2)
      time <- paste0(time, "}")}

    data <- results$find(query = time)
  } else {
   data <- results$find()
  }
}
