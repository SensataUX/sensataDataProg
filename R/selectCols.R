# selectCols.R V1
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
#' @param dropTotalTime if TRUE it drops totalTimeMin variable.
#' @param dropQuestionTime if TRUE drops q_time variables
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe ready for client.
#' @keywords sensata microdata metadata data-cleaning
#' @import tidyverse
#'
#' @examples
#' bogData1 <- bogData1 %>% scrubData(dup = F, ageVar = "EVCS2", ageVal = "Menos de 18 años", geoloc = T)
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

  dropVec <- vector()
  if(dropGeo && geoCoordinates){
    dropVec <- c("geolocation.coordinates")
  }
  if(dropGeo && !geoCoordinates){
    dropVec <- c("lat", "long")
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
  df <- df %>% select(-c(all_of(dropVec)))

  return(df)
}

