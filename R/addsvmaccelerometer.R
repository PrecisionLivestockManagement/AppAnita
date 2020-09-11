#' Add GPS data to the AnitaGPS collection in the PLMResearch database.
#'
#' This function adds GPS data from Anita's Belmont trial to the MongoDB database.
#' @name addsvmaccelerometer
#' @param RFID a list of cattle RFID number/s
#' @param mtag a list of cattle management tag number/s
#' @param date the date of the coordinate point
#' @param hour the hour of the coordinate point
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
#' @param status the calving status of the cow
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addsvmaccelerometer <- function(RFID, mtag, date, hour, period, hour.mean.MI, hour.mean.diff.Z, hour.max.diff.Z, hour.max.MV, self.hour.min.diff.all,
                                self.hour.min.MV, self.hour.min.diff.entropy, self.hour.sd.diff.all, self.hour.range.X, self.hour.range.energy,
                                self.hour.range.diff.X, self.hour.range.diff.Z, self.hour.range.diff.all, self.hour.range.MV, day.mean.X, day.max.diff.Y,
                                day.max.diff.Z, day.max.MV, herd.day.mean.Z, herd.day.mean.all, herd.day.mean.MI, herd.day.mean.entropy, herd.day.min.diff.Y,
                                herd.day.min.diff.Z, herd.day.max.diff.X, herd.day.sd.X, herd.day.sd.Y, herd.day.sd.Z, herd.day.sd.all, herd.day.sd.energy,
                                herd.day.sd.diff.Y, herd.day.range.X, herd.day.range.diff.all, self.day.mean.X, self.day.mean.Y, self.day.mean.all,
                                self.day.mean.MI, self.day.mean.diff.X, self.day.min.diff.Y, self.day.min.diff.Z, self.day.min.diff.all, self.day.range.diff.Y,
                                self.day.range.diff.Z, status, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  pass <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  calvmod <- mongo(collection = "AnitaSVMAccelerometer", db = "PLMResearch", url = pass, verbose = T)

  calvmoddata <- sprintf(
    '{"RFID":"%s", "management":"%s", "date":"%s", "hour":{"$date":"%s"}, "period":"%s", "hourmeanMI":%s, "hourmeandiffZ":%s,
    "hourmaxdiffZ":%s, "hourmaxMV":%s, "selfhourmindiffall":%s, "selfhourminMV":%s, "selfhourmindiffentropy":%s,
    "selfhoursddiffall":%s, "selfhourrangeX":%s, "selfhourrangeenergy":%s, "selfhourrangediffX":%s, "selfhourrangediffZ":%s,
    "selfhourrangediffall":%s, "selfhourrangeMV":%s, "daymeanX":%s, "daymaxdiffY":%s, "daymaxdiffZ":%s, "daymaxMV":%s,
    "herddaymeanZ":%s, "herddaymeanall":%s, "herddaymeanMI":%s, "herddaymeanentropy":%s, "herddaymindiffY":%s,
    "herddaymindiffZ":%s, "herddaymaxdiffX":%s, "herddaysdX":%s, "herddaysdY":%s, "herddaysdZ":%s, "herddaysdall":%s,
    "herddaysdenergy":%s, "herddaysddiffY":%s, "herddayrangeX":%s, "herddayrangediffall":%s, "selfdaymeanX":%s,
    "selfdaymeanY":%s, "selfdaymeanall":%s, "selfdaymeanMI":%s, "selfdaymeandiffX":%s, "selfdaymindiffY":%s,
    "selfdaymindiffZ":%s, "selfdaymindiffall":%s, "selfdayrangediffY":%s, "selfdayrangediffZ":%s, "status":"%s"}',
    RFID, mtag, date, paste0(substr(hour,1,10),"T",substr(hour,12,19),"+1000"), period, hour.mean.MI, hour.mean.diff.Z, hour.max.diff.Z, hour.max.MV,
    self.hour.min.diff.all, self.hour.min.MV, self.hour.min.diff.entropy, self.hour.sd.diff.all, self.hour.range.X, self.hour.range.energy,
    self.hour.range.diff.X, self.hour.range.diff.Z, self.hour.range.diff.all, self.hour.range.MV, day.mean.X, day.max.diff.Y, day.max.diff.Z, day.max.MV,
    herd.day.mean.Z, herd.day.mean.all, herd.day.mean.MI, herd.day.mean.entropy, herd.day.min.diff.Y, herd.day.min.diff.Z, herd.day.max.diff.X,
    herd.day.sd.X, herd.day.sd.Y, herd.day.sd.Z, herd.day.sd.all, herd.day.sd.energy, herd.day.sd.diff.Y, herd.day.range.X, herd.day.range.diff.all,
    self.day.mean.X, self.day.mean.Y, self.day.mean.all, self.day.mean.MI, self.day.mean.diff.X, self.day.min.diff.Y, self.day.min.diff.Z,
    self.day.min.diff.all, self.day.range.diff.Y, self.day.range.diff.Z, status)

  calvmod$insert(calvmoddata)
}
