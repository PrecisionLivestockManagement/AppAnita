#' Add cattle data to the AnitaCattle collection in the PLMResearch database.
#'
#' This function adds cattle data from Anita's Belmont trial to the MongoDB database.
#' @name addcattle
#' @param RFID a list of cattle RFIDs
#' @param mtag a list of cattle management tags
#' @param calvingdate a list of cattle calving dates
#' @param calving a list of calving styles
#' @param paddock a list of cattle paddocks
#' @param date a list of dates for the data
#' @param hour a list of hours for the data
#' @param status a list of cow statuses for the given timestamp
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addcattle <- function(RFID, mtag, calvingdate, calving, paddock, date, hour, status, username = user, password = pass){
  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  cattle <- mongo(collection = "AnitaCattle", db = "PLMResearch", url = pass, verbose = T)

  cattledata <- sprintf(
    '{"RFID":"%s", "management":"%s", "calvingdate":"%s", "calving":"%s", "paddock":"%s", "date":"%s", "hour":{"$date":"%s"}, "status":"%s"}',
    RFID, mtag, calvingdate, calving, paddock, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), status)

  cattle$insert(cattledata)

}
