#' Retrieves paddock data from the AnitaPaddocks collection in the PLMResearch database.
#'
#' This function retrieves paddock data from Anita's Belmont trial to the MongoDB database.
#' @name getpaddocks
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return returns the paddock spatial information
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import sp
#' @export


getpaddocks <- function(username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddocks <- mongo(collection = "AnitaPaddocks", db = "PLMResearch", url = pass, verbose = T)

  fields <- sprintf('{"paddname":true, "hectares":true, "cattle":true, "_id":false}')

  info <- paddocks$find(fields = fields)

  return(info)

}
