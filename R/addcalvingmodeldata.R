#' Add data that feeds into the calvingmodel function to the AnitaCalvingModel collection in the PLMResearch database.
#'
#' This function adds data that feeds into the calvingmodel function from Anita's Belmont trial to the MongoDB database.
#' @name addcalvingmodeldata
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date a list of cattle dates
#' @param hour a list of cattle hours
#' @param hour.mean.MI
#' @param hour.mean.diff.Z
#' @param hour.max.diff.Z
#' @param hour.max.MV
#' @param self.hour.min.diff.all
#' @param self.hour.min.MV
#' @param self.hour.min.diff.entropy
#' @param self.hour.sd.diff.all
#' @param self.hour.range.X
#' @param self.hour.range.energy
#' @param self.hour.range.diff.X
#' @param self.hour.range.diff.Z
#' @param self.hour.range.diff.all
#' @param self.hour.range.MV
#' @param day.mean.X
#' @param day.max.diff.Y
#' @param day.max.diff.Z
#' @param day.max.MV
#' @param herd.day.mean.Z
#' @param herd.day.mean.all
#' @param herd.day.mean.MI
#' @param herd.day.mean.entropy
#' @param herd.day.min.diff.Y
#' @param herd.day.min.diff.Z
#' @param herd.day.max.diff.X
#' @param herd.day.sd.X
#' @param herd.day.sd.Y
#' @param herd.day.sd.Z
#' @param herd.day.sd.all
#' @param herd.day.sd.energy
#' @param herd.day.sd.diff.Y
#' @param herd.day.range.X
#' @param herd.day.range.diff.all
#' @param self.day.mean.X
#' @param self.day.mean.Y
#' @param self.day.mean.all
#' @param self.day.mean.MI
#' @param self.day.mean.diff.X
#' @param self.day.min.diff.Y
#' @param self.day.min.diff.Z
#' @param self.day.min.diff.all
#' @param self.day.range.diff.Y
#' @param self.day.range.diff.Z
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
#' @param hour.rain
#' @param hour.humidity
#' @param hour.THI
#' @param day.minrain
#' @param day.rangerain
#' @param day.maxhumidity
#' @param day.meanhumidity
#' @param day.minTHI
#' @param day.meanTHI
#' @param day.sdTHI
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import dplyr
#' @import mongolite
#' @export


addcalvingmodeldata <- function(RFID, mtag, date, hour, hour.mean.MI, hour.mean.diff.Z, hour.max.diff.Z, hour.max.MV, self.hour.min.diff.all,
                            self.hour.min.MV, self.hour.min.diff.entropy, self.hour.sd.diff.all, self.hour.range.X, self.hour.range.energy,
                            self.hour.range.diff.X, self.hour.range.diff.Z, self.hour.range.diff.all, self.hour.range.MV, day.mean.X, day.max.diff.Y,
                            day.max.diff.Z, day.max.MV, herd.day.mean.Z, herd.day.mean.all, herd.day.mean.MI, herd.day.mean.entropy, herd.day.min.diff.Y,
                            herd.day.min.diff.Z, herd.day.max.diff.X, herd.day.sd.X, herd.day.sd.Y, herd.day.sd.Z, herd.day.sd.all, herd.day.sd.energy,
                            herd.day.sd.diff.Y, herd.day.range.X, herd.day.range.diff.all, self.day.mean.X, self.day.mean.Y, self.day.mean.all,
                            self.day.mean.MI, self.day.mean.diff.X, self.day.min.diff.Y, self.day.min.diff.Z, self.day.min.diff.all, self.day.range.diff.Y,
                            self.day.range.diff.Z, MCP.herd.day.MCP, MCP.self.day.MCP, MMM.day.mean.dist, MMM.day.mean.diff.dist, MMM.self.day.mean.dist,
                            MMM.self.day.sd.diff.speed, nearest.day.mean.diff, nearest.day.min.diff, nearest.day.max.diff, nearest.self.day.sd.diff,
                            hour.rain, hour.humidity, hour.THI, day.minrain, day.rangerain, day.maxhumidity, day.meanhumidity, day.minTHI, day.meanTHI, day.sdTHI,
                            username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  calvmod <- mongo(collection = "AnitaCalvingModelData", db = "PLMResearch", url = pass, verbose = T)

  calvmoddata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "hour":{"$date":"%s"}, "hourmeanMI":%s, "hourmeandiffZ":%s,
    "hourmaxdiffZ":%s, "hourmaxMV":%s, "selfhourmindiffall":%s, "selfhourminMV":%s, "selfhourmindiffentropy":%s,
    "selfhoursddiffall":%s, "selfhourrangeX":%s, "selfhourrangeenergy":%s, "selfhourrangediffX":%s, "selfhourrangediffZ":%s,
    "selfhourrangediffall":%s, "selfhourrangeMV":%s, "daymeanX":%s, "daymaxdiffY":%s, "daymaxdiffZ":%s, "daymaxMV":%s,
    "herddaymeanZ":%s, "herddaymeanall":%s, "herddaymeanMI":%s, "herddaymeanentropy":%s, "herddaymindiffY":%s,
    "herddaymindiffZ":%s, "herddaymaxdiffX":%s, "herddaysdX":%s, "herddaysdY":%s, "herddaysdZ":%s, "herddaysdall":%s,
    "herddaysdenergy":%s, "herddaysddiffY":%s, "herddayrangeX":%s, "herddayrangediffall":%s, "selfdaymeanX":%s,
    "selfdaymeanY":%s, "selfdaymeanall":%s, "selfdaymeanMI":%s, "selfdaymeandiffX":%s, "selfdaymindiffY":%s,
    "selfdaymindiffZ":%s, "selfdaymindiffall":%s, "selfdayrangediffY":%s, "selfdayrangediffZ":%s, "MCPherddayMCP":%s,
    "MCPselfdayMCP":%s, "MMMdaymeandist":%s, "MMMdaymeandiffdist":%s, "MMMselfdaymeandist":%s, "MMMselfdaysddiffspeed":%s,
    "nearestdaymeandiff":%s, "nearestdaymindiff":%s, "nearestdaymaxdiff":%s, "nearestselfdaysddiff":%s, "hourrain":%s,
    "hourhumidity":%s, "hourTHI":%s, "dayminrain":%s, "dayrangerain":%s, "daymaxhumidity":%s, "daymeanhumidity":%s, "dayminTHI":%s,
    "daymeanTHI":%s, "daysdTHI":%s}',
    RFID, mtag, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), hour.mean.MI, hour.mean.diff.Z, hour.max.diff.Z, hour.max.MV,
    self.hour.min.diff.all, self.hour.min.MV, self.hour.min.diff.entropy, self.hour.sd.diff.all, self.hour.range.X, self.hour.range.energy,
    self.hour.range.diff.X, self.hour.range.diff.Z, self.hour.range.diff.all, self.hour.range.MV, day.mean.X, day.max.diff.Y, day.max.diff.Z, day.max.MV,
    herd.day.mean.Z, herd.day.mean.all, herd.day.mean.MI, herd.day.mean.entropy, herd.day.min.diff.Y, herd.day.min.diff.Z, herd.day.max.diff.X,
    herd.day.sd.X, herd.day.sd.Y, herd.day.sd.Z, herd.day.sd.all, herd.day.sd.energy, herd.day.sd.diff.Y, herd.day.range.X, herd.day.range.diff.all,
    self.day.mean.X, self.day.mean.Y, self.day.mean.all, self.day.mean.MI, self.day.mean.diff.X, self.day.min.diff.Y, self.day.min.diff.Z,
    self.day.min.diff.all, self.day.range.diff.Y, self.day.range.diff.Z, MCP.herd.day.MCP, MCP.self.day.MCP, MMM.day.mean.dist, MMM.day.mean.diff.dist,
    MMM.self.day.mean.dist, MMM.self.day.sd.diff.speed, nearest.day.mean.diff, nearest.day.min.diff, nearest.day.max.diff, nearest.self.day.sd.diff,
    hour.rain, hour.humidity, hour.THI, day.minrain, day.rangerain, day.maxhumidity, day.meanhumidity, day.minTHI, day.meanTHI, day.sdTHI)

  calvmod$insert(calvmoddata)

}
