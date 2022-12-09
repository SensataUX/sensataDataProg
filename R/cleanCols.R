# cleanCols.R V1
# Description: This function eliminates the unnecesary metadata columns and renames the columns from mongo to the identifier, according to the dictionary.
# Created by: Gabriel N. Camargo-Toledo
# Created on: Jan/19/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Oct/13/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.1.1
# Requires: tidyverse, labelled
# Input: data from sensata platform using contentful+mongoDb. Latest data architecture of newResponses or structuredResponses.
# Input: Dictionary created using dictGenerator.R
# Output: microdata as an object in the current R session.

# cleanCols ---------------------------------------------------------------

#' Function to clean sensata data
#'
#' This function eliminates the unnecesary metadata columns and renames the columns from mongo to the identifier, according to the dictionary.
#' @param df data downloaded from Mongo and loaded to R.
#' @param dictionary dictionary created with dictGenerator.R
#' @param colsToKeep vector of names of metadata columns to keep. Defaults to: id, surveyName, surveyId, totalTimeMin, geolocation.coordinates, fingerprint, browserReport.ip and, createdAt. All questions are always included.
#' @param removeParams logical, if TRUE params from mongo are removed.
#' @param removeScreens logical, if TRUE  screens are removed
#' @param responseType which object version should the function use, one of "newResponses" or "structuredResponses"
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with renamed fewer metadata and renamed selected answers as the question identifier.
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' bogData <- bogData %>% cleanCols(dictionary = bogDic)
#' @export


cleanCols <- function(df, dictionary, colsToKeep = c("id",
                                                     "surveyName",
                                                     "surveyId",
                                                     "totalTimeMin",
                                                     "geolocation.coordinates",
                                                     "lat",
                                                     "long",
                                                     "geo.accuracy",
                                                     "fingerprint",
                                                     "browserReport.ip",
                                                     "sensataId",
                                                     "createdAt"),
                      removeParams = FALSE,
                      removeScreens = TRUE,
                      responseType = "newResponses"){


  # Rename _id
  df <- df %>% rename(id = `_id`)


  # Select columns --------------------------------
  df <- df %>%
    select(any_of(colsToKeep),
           starts_with("params"),
           starts_with(responseType),
           ends_with(".timeToCompletion")) %>%
    select(all_of(colsToKeep),
           starts_with("params"),
           contains("selected"),
           ends_with(".timeToCompletion")) %>%
    select(where(not_all_na))

  if (removeParams) {
    df <- df %>% select(-starts_with('params'))
  }


  # Make sure all time columns are double -----------------------------------
  df <- df %>% mutate(across(contains("time"), as.double))


  # Rename columns --------------------------------------------------------
  df <- df %>% rename_with( ~ gsub(".timeToCompletion", "_time",.x, fixed = T))
  df <- df %>% rename_with( ~ gsub(paste0(responseType, "."), "",.x, fixed = T))
  df <- df %>% rename_with( ~ gsub(".selected", "",.x, fixed = T))

  # Drop the other responseType ------------------------------------------
  if (responseType=="structuredResponses"){
    df <- df %>% select(!(contains("newResponses")))
  }
  if (responseType=="newResponses"){
    df <- df %>% select(!(contains("structuredResponses")))
  }

  # rename as identifier codes according to dictionary
  codList <- dictionary %>%
    filter(type != "conjoint") %>%
    select("id", "identifier") %>%
    unique() %>%
    pivot_wider(names_from = identifier, values_from = id) %>%
    as.list()
  df <- df %>% rename(!!! codList)

  # rm(codList)

  # Remove screens ---------------------------------------------------
  if(removeScreens){
    screensVec <- dictionary[["identifier"]][dictionary[["type"]]=="screen"]
    df <- df %>% select(-(all_of(screensVec)))
  }

  # output ------------------------------------------------------------------
  return(df)

}
