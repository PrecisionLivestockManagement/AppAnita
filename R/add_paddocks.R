#' Add paddock data to the AnitaPaddocks collection in the PLMResearch database.
#'
#' This function adds paddock data from Anita's Belmont trial to the MongoDB database.
#' @name add_paddocks
#' @param paddname the name of the paddock
#' @param hectares the hectares of the paddock
#' @param cattle the number of cattle in the paddock
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


add_paddocks <- function(paddname, hectares, cattle, username, password){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  paddock <- mongo(collection = "AnitaPaddocks", db = "PLMResearch", url = pass, verbose = T)

  paddockdata <- sprintf(
    '{"paddname":"%s", "hectares":%s, "cattle":%s}',
    paddname, hectares, cattle)

  paddock$insert(paddockdata)
}
