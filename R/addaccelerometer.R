#' Add accelerometer data to the AnitaAccelerometer collection in the PLMResearch database.
#'
#' This function adds accelerometer data from Anita's Belmont trial to the MongoDB database.
#' @name addaccelerometer
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date a list of cattle dates
#' @param hour a list of cattle hours
#' @param roundedtime a list of cattle rounded times
#' @param timestamp a list of cattle timestamps
#' @param X the x axes
#' @param Y the Y axes
#' @param Z the Z axes
#' @param status the status
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import dplyr
#' @import mongolite
#' @export


addaccelerometer <- function(RFID, mtag, date, time, mean.MI, min.MI, max.MI, sd.MI, range.MI, mean.MV, min.MV, max.MV, sd.MV, range.MV, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  acccoll <- mongo(collection = "AnitaAccelerometer", db = "PLMResearch", url = url, verbose = T)

  accdata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "time":{"$date":"%s"},
    "meanMI":%s, "minMI":%s, "maxMI":%s, "sdMI":%s, "rangeMI":%s,
    "meanMV":%s, "minMV":%s, "maxMV":%s, "sdMV":%s, "rangeMV":%s}',
    RFID, mtag, date, paste0(substr(time,1,10),"T",substr(time,12,19),"+1000"),
    mean.MI, min.MI, max.MI, sd.MI, range.MI, mean.MV, min.MV, max.MV, sd.MV, range.MV)

  acccoll$insert(accdata)
}
