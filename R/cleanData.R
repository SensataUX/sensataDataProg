#' Function to clean sensata data
#'
#' This function eliminates the unnecesary metadata columns and renames the columns from mongo to the identifier, according to the dictionary.
#' @param data data downloaded from Mongo and loaded to R.
#' @param dictionary dictionary created with dictGenerator.R
#' @param keep vector of names of metadata columns to keep. Defaults to: id, surveyName, surveyId, totalTimeMin, geolocation.coordinates, fingerprint, browserReport.ip and, createdAt. All questions are always included.
#' @param params logical, if F params from mongo are erased.
#' @param skipVal Text that should have all the skipped questions. defaults to "Saltar pregunta"
#' 
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary and exported to .csv and .Rdata useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' cleanData(dictionary = bogExpDic,skipVal = "Probando skipVal")
#' @export


# cleanData

cleanData <- function(data, dictionary, keep = c("id", 
                                                 "surveyName", 
                                                 "surveyId", 
                                                 "totalTimeMin", 
                                                 "geolocation.coordinates", 
                                                 "fingerprint", 
                                                 "browserReport.ip",
                                                 "sensataId",
                                                 "createdAt"),
                      params = T,
                      skipVal = "Saltar pregunta"){
  
  # Loading packages
  require(tidyverse)
  
  # Rename _id
  data <- data %>% rename(id = `_id`)
  
  
  # Selecting columns --------------------------------
  if(params){
    data <- data %>% select(all_of(keep),
                            starts_with("params"),
                            ends_with(".selected"), 
                            ends_with(".timeToCompletion"))
  }else{
    data <- data %>% select(all_of(keep),
                            ends_with(".selected"), 
                            ends_with(".timeToCompletion"))
  }
  

  # Renaming columns --------------------------------------------------------

  data <- data %>% rename_with( ~ gsub(".timeToCompletion", "_time",.x, fixed = T))
  data <- data %>% rename_with( ~ gsub("newResponses.", "",.x, fixed = T))
  data <- data %>% rename_with( ~ gsub(".selected", "",.x, fixed = T))
  
  # renaming to codes according to dictionary
  
  codList <- dictionary %>% 
    select("qid", "identifier") %>% 
    unique() %>%
    pivot_wider(names_from = identifier, values_from = qid) %>%
    as.list()
  data <- data %>% rename(!!! codList)
  
  rm(codList)
  

  # Change NS-NR, false and true --------------------------------------------
  data["createdAt"] <- as.character(data["createdAt"])
  
  data[data == "true"] <- "1"
  data[data == "false"] <- "0"
  data[data == "NS-NR"] <- skipVal
  data[data == "S99"] <- skipVal

  # output ------------------------------------------------------------------
  return(data)
  
}
