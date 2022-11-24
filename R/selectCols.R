# s electCols.R V1
# Description: This function eliminates the unnecesary metadata columns and renames the columns from mongo to the identifier, according to the dictionary.
# Created by: Gabriel N. Camargo-Toledo
# Created on: Jan/19/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Oct/13/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.1.1
# Requires: tidyverse, labelled
# Input: data from sensata platform using contentful+mongoDb. Latest data architecture of newResponses.
# Input: Dictionary created using dictGenerator.R
# Output: microdata as an object in the current R session.

# selectCols --------------------------------------------------------------

#' Function to export sensata data for final customer
#'
#' This function eliminates most metadata from the dataset. This is basically a wrapper for tidyverse::select()
#' @param df data downloaded from Mongo, cleaned with cleanData.R, scrubbed with scrubData.R and prepared with factorSensata.R.
#' @param dropGeo if TRUE will drop coordinates (lat & long) and geo.accuracy
#' @param geoCoordinates if TRUE will drop geo.coordinates columns, instead of lat & long
#' @param dropParams if TRUE will drop all columns that start with params
#' @param dropUserData if TRUE will drop fingerprint and sensataId
#' @param dropMetaData if TRUE will drop createdAt, surveyId and surveyName
#' @param dropTotalTime if TRUE it drops totalTimeMin column
#' @param dropQuestionTime if TRUE drops q_time columns
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe ready for client.
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' TBD
#' @export
#'
# REV: no veo muy bien la necesidad de tener este archivo, o al menos no de esta forma... lo ve´ria más útil si agrupamos así:
# dropGeo, dropParams, dropUserdata (browserReport, fingerprint, token, captchaScore), dropMetadata (createdAt, updatedAt, ObjectId, id), dropQuestionTime (), dropSurveyMeta (surveyID, surveName)

selectCols <- function(df,
                       dropGeo = T,
                       geoCoordinates = F,
                       dropParams = T,
                       dropUserData = T,
                       dropMetaData = T,
                       dropTotalTime = T,
                       dropQuestionTime = T){
  if(dropGeo == FALSE && geoCoordinates == T){
    rlang::warn("You set geoCoordinates to TRUE, but you said you didn't want to drop geo data, check what you want")
  }
  dropVec <- vector()
  if(dropGeo && geoCoordinates){
    dropVec <- c("geolocation.coordinates", "geo.accuracy")
  }
  if(dropGeo && !geoCoordinates){
    dropVec <- c("lat", "long", "geo.accuracy")
  }
  if(dropParams){
    df <- df %>% select(!(starts_with('params')))
  }
  if(dropUserData){
    dropVec <- c(dropVec, "fingerprint","sensataId")
    df <- df %>% select(!(starts_with('browser')))
  }
  if(dropMetaData){
    dropVec <- c(dropVec, "createdAt", "surveyId", "surveyName")
  }
  if(dropTotalTime){
    dropVec <- c(dropVec, "totalTimeMin")
  }
  if(dropQuestionTime){
    df <- df %>% select(!(ends_with("_time")))
  }
  if(!is_empty(dropVec)){
    df <- df %>% select(-all_of(dropVec))
  }


  return(df)
}

