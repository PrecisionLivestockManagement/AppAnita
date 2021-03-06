#' Add cattle data to the AnitaCattle collection in the PLMResearch database.
#'
#' This function adds cattle data from Anita's Belmont trial to the MongoDB database.
#' @name addnotes
#' @param RFID a list of cattle RFIDs
#' @param username username for use with Anita's App
#' @param password password for use with Anita's App
#' @return a message that indicates the data has been successfully added
#' @author Anita Chang \email{a.chang@@cqu.edu.au}
#' @import mongolite
#' @export


addnotes <- function(RFID, calfID = NULL, calfsex = NULL, calfvigour = NULL, calfwt = NULL, udder = NULL, front = NULL, rear = NULL, cowbcs = NULL, notes = NULL, username = user, password = pass){

  if(is.null(username)||is.null(password)){
    username = keyring::key_list("DMMongoDB")[1,2]
    password =  keyring::key_get("DMMongoDB", username)}

  url <- sprintf("mongodb://%s:%s@datamuster-shard-00-00-8mplm.mongodb.net:27017,datamuster-shard-00-01-8mplm.mongodb.net:27017,datamuster-shard-00-02-8mplm.mongodb.net:27017/test?ssl=true&replicaSet=DataMuster-shard-0&authSource=admin", username, password)
  comments <- mongo(collection = "AnitaComments", db = "PLMResearch", url = url, verbose = T)

  for (i in 1:length(RFID)){
    RFIDS <- sprintf('{"RFID":"%s"}', RFID[i])
    if(!is.null(calfID)){
      IDI <- sprintf('{"$set":{"calfID":"%s"}}', calfID[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(calfsex)){
      IDI <- sprintf('{"$set":{"calfsex":"%s"}}', calfsex[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(calfvigour)){
      IDI <- sprintf('{"$set":{"calfvigour":"%s"}}', calfvigour[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(calfwt)){
      IDI <- sprintf('{"$set":{"calfwt":"%s"}}', calfwt[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(udder)){
      IDI <- sprintf('{"$set":{"udder":"%s"}}', udder[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(front)){
      IDI <- sprintf('{"$set":{"front":"%s"}}', front[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(rear)){
      IDI <- sprintf('{"$set":{"rear":"%s"}}', rear[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(cowbcs)){
      IDI <- sprintf('{"$set":{"cowbcs":"%s"}}', cowbcs[i])
      comments$update(RFIDS, IDI)
    }
    if(!is.null(notes)){
      IDI <- sprintf('{"$set":{"notes":"%s"}}', notes[i])
      comments$update(RFIDS, IDI)
    }
  }
}
