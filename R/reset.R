#' Retrieves weather data from the AnitaWeather collection in the PLMResearch database.
#'
#' This function retrieves weather data from Anita's Belmont trial to the MongoDB database.
#' @name reset
#' @param status the status
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return returns the paddock spatial information
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @import dplyr
#' @import rgdal
#' @export


reset <- function(username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  comm <- mongo(collection = "AnitaComments", db = "PLMResearch", url = url, verbose = T)
  mlres <- mongo(collection = "AnitaMLResults", db = "PLMResearch", url = url, verbose = T)
  stat <- mongo(collection = "AnitaStatus", db = "PLMResearch", url = url, verbose = T)

  # Comments
  commdata <- comm$find()
  commdata[,c(3:11)] <- ""
  for(i in 1:nrow(commdata)){
    RFIDS <- sprintf('{"RFID":"%s"}', commdata$RFID[i])
    IDI <- sprintf('{"$set":{"calfID":"%s", "calfsex":"%s", "calfvigour":"%s", "calfwt":"%s", "udder":"%s", "front":"%s", "rear":"%s", "cowbcs":"%s", "notes":"%s"}}',
                   commdata$calfID[i], commdata$calfsex[i], commdata$calfvigour[i], commdata$calfwt[i], commdata$udder[i], commdata$front[i], commdata$rear[i],
                   commdata$cowbcs[i], commdata$notes[i])
    comm$update(RFIDS, IDI)
  }

  # ML results
  mlresdata <- mlres$find()
  mlresdata <- mlresdata %>%
    filter(incorrectalert == "pregnant")
  if(nrow(mlresdata) > 0){
    mlresdata$incorrectalert <- ""
    for (i in 1:nrow(mlresdata)){
      RFIDS <- sprintf('{"RFID":"%s", "time":{"$date":"%s"}}', mlresdata$RFID[i], strftime(as.POSIXct(paste0(mlresdata$time[i])), format="%Y-%m-%dT%H:%M:%OSZ", tz = "GMT"))
      IDI <- sprintf('{"$set":{"incorrectalert":"%s"}}', mlresdata$incorrectalert[i])
      mlres$update(RFIDS, IDI)
      }
  }

  # Status
  statdata <- stat$find()
  statdata$status <- "Pregnant"
  statdata$notifications <- "On"
  statdata$calvingdate <- ""
  for(i in 1:nrow(statdata)){
    RFIDS <- sprintf('{"RFID":"%s"}', statdata$RFID[i])
    IDI <- sprintf('{"$set":{"status":"%s", "notifications":"%s", "calvingdate":"%s"}}',
                   statdata$status[i], statdata$notifications[i], statdata$calvingdate[i])
    stat$update(RFIDS, IDI)
  }
}
