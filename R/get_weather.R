#' Retrieves cattle from the DataMuster database
#'
#' This function retreives a list of cattle, their paddock allocations and spatial coordinates for display on the property map on the DataMuster website
#' @name get_weather
#' @param property the name of the property to search the database
#' @param sex the sex of the cattle to be returned, determined by the "Males or Females" filter
#' @param category the category of cattle to be returned, determined by the "Breeders or Growers" filter
#' @param zoom indicates whether to return cattle from the whole property or to filter cattle by paddock, determined by the "Paddock Groups" filter
#' @param paddock the paddock allocation of the cattle to be returned, determined by selecting a paddock on the map
#' @param username a username to access the DataMuster database
#' @param password a password to access the DataMuster database
#' @return a dataframe with a list of cattle numbers by paddock
#' @author Dave Swain \email{d.swain@@cqu.edu.au} and Lauren O'Connor \email{l.r.oconnor@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


get_weather <- function(timestamp, username = username, password = password){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  weather <- mongo(collection = "AnitaWeather", db = "PLMResearch", url = pass, verbose = T)

  # timestamp <- "2018-10-01 13:00:00"
  timestamp <- paste(unlist(timestamp), collapse = '", "')
  timestamp <- sprintf('"timestamp":{"$date":"%s"},', strftime(as.POSIXct(paste0(timestamp)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  # Set up query to search for cattle
  filter <- paste0("{", timestamp,"}")
  if(nchar(filter)==2){}else{
    filter <- substr(filter, 1 , nchar(filter)-2)
    filter <- paste0(filter, "}")}
  fields <- sprintf('{"timestamp":true, "rain":true, "temperature":true, "humidity":true, "THI":true, "_id":false}')
  info <- weather$find(query = filter, fields = fields)

  return(info)

}
