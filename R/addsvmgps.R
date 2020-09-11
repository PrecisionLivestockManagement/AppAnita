#' Add GPS data to the AnitaGPS collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name addgps
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date the date of the coordinate point
#' @param hour the hour of the coordinate point
#' @param MCP.herd.day.MCP
#' @param MCP.self.day.MCP
#' @param MMM.day.mean.dist
#' @param MMM.day.mean.diff.dist
#' @param MMM.self.day.mean.dist
#' @param MMM.self.day.sd.diff.speed
#' @param nearest.day.mean.diff
#' @param nearest.day.min.diff
#' @param nearest.day.max.diff
#' @param nearest.self.day.sd.diff
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addgps <- function(RFID, mtag, date, hour, period, MCP.herd.day.MCP, MCP.self.day.MCP, MMM.day.mean.dist, MMM.day.mean.diff.dist, MMM.self.day.mean.dist,
                   MMM.self.day.sd.diff.speed, nearest.day.mean.diff, nearest.day.min.diff, nearest.day.max.diff, nearest.self.day.sd.diff, status,
                   username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  calvmod <- mongo(collection = "AnitaSVMGPS", db = "PLMResearch", url = pass, verbose = T)

  calvmoddata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "hour":{"$date":"%s"}, "period":"%s", "MCPherddayMCP":%s,
    "MCPselfdayMCP":%s, "MMMdaymeandist":%s, "MMMdaymeandiffdist":%s, "MMMselfdaymeandist":%s, "MMMselfdaysddiffspeed":%s,
    "nearestdaymeandiff":%s, "nearestdaymindiff":%s, "nearestdaymaxdiff":%s, "nearestselfdaysddiff":%s, "status":"%s"}',
    RFID, mtag, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), period, MCP.herd.day.MCP, MCP.self.day.MCP, MMM.day.mean.dist,
    MMM.day.mean.diff.dist, MMM.self.day.mean.dist, MMM.self.day.sd.diff.speed, nearest.day.mean.diff, nearest.day.min.diff,
    nearest.day.max.diff, nearest.self.day.sd.diff, status)

  calvmod$insert(calvmoddata)
}
