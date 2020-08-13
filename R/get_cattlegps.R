#' Retrieves cattle from the DataMuster database
#'
#' This function retreives a list of cattle, their paddock allocations and spatial coordinates for display on the property map on the DataMuster website
#' @name get_cattlegps
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


get_cattlegps <- function(roundedtime, status, username = username, password = password){

  username = keyring::key_list("DMMongoDB")[1,2]
  password =  keyring::key_get("DMMongoDB", username)

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  GPS <- mongo(collection = "AnitaGPS", db = "PLMResearch", url = pass, verbose = T)

  roundedtime <- "2018-10-02 13:00:00"
  roundedtime <- paste(unlist(roundedtime), collapse = '", "')
  roundedtime <- sprintf('"roundedtime":{"$date":"%s"},', strftime(as.POSIXct(paste0(roundedtime)), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))

  status <- "preg"
  status <- paste(unlist(status), collapse = '", "')
  status <- sprintf('"status":{"$in":["%s"]},', status)

  # Set up query to search for cattle
  filter <- paste0("{", roundedtime, status, "}")
  if(nchar(filter)==2){}else{
    filter <- substr(filter, 1 , nchar(filter)-2)
    filter <- paste0(filter, "}")}
  fields <- sprintf('{"roundedtime":true, "status":true, "RFID":true, "Management":true, "lat":true, "long":true, "paddock":true, "_id":false}')
  info <- GPS$find(query = filter, fields = fields)

  colnames(info)[4:6] <- c("cowlat", "cowlong", "Paddock")

  paddocks1 <- DMApp::appgetpaddocks(property = "Belmont", username = username, password = password)

  for(i in 1:nrow(paddocks1@data)){

    lat <- paddocks1@polygons[[i]]@labpt[1]
    long <- paddocks1@polygons[[i]]@labpt[2]

    paddocks1$lat[i] <- paddocks1@polygons[[i]]@labpt[1]
    paddocks1$long[i] <- paddocks1@polygons[[i]]@labpt[2]

  }

  paddocks1 <- data.frame(paddocks1 %>%
               select(paddname, lat, long)) %>%
               rename(Paddock = paddname)

  cattlepaddata <- data.frame(left_join(info, paddocks1, by = "Paddock") %>%
                   filter(Paddock != "xxxxxx"))
  colnames(cattlepaddata)[8:9] <- c("pdklat", "pdklong")

  return(cattlepaddata)

}
