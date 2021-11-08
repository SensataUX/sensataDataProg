# extractMetaData.R V0.0.2
# Created by: Gabriel N. Camargo-Toledo
# Created on: Oct/27/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Oct/27/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.1.2
# Requires: tidyverse
# Input: Interim data after scrubRows.R
# Input: Dictionary created using dictGenerator.R
# Output: table for columns A to H (except lang) from file https://docs.google.com/spreadsheets/d/1CdSqvX3Hhayk_NGvr9eqDeAlA8XG9BX_-Exm5K3RKlI/edit#gid=2106088199

#' Extract sensata metadata
#'
#' This function creates table to fill out metadata on https://docs.google.com/spreadsheets/d/1CdSqvX3Hhayk_NGvr9eqDeAlA8XG9BX_-Exm5K3RKlI/edit#gid=2106088199
#' @param df Interim dataset after scrubRows.r
#' @param dict Dictionary created using dictGenerator.R
#' @param stage pilot or live collection for params.utm_campaign
#' @param source wa, fb, email, etc for params.utm_source
#' @param medium cpc, referral, etc for params.utm_medium
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return 1-row metadata for columns A to H (except lang)
#' @keywords metadata sensataDataProg extract
#'
#'
#' @examples
#' extractMetaData(data$q1time)
#' @export
#'

extractMetaData <- function(df = intData,
                            dict = dict,
                            stage = NULL,
                            source = NULL,
                            medium = NULL){
  rlang::inform("This function assumes that the last individual of selected params has the correct surveyName for the campaign column")

  # filter by params -----
  if(!is.null(stage)){
    df <- df %>% filter(params.utm_campaign == stage)
  }
  if(!is.null(source)){
    df <- df %>% filter(params.utm_source == source)
  }
  if(!is.null(medium)){
    df <- df %>% filter(params.utm_medium == medium)
  }

  # Create columns -------
  Campaign <- tail(df$surveyName, 1)
  SurveyID <- tail(df$surveyId, 1)
  NQuestions <- max(as.integer(dict$order))+1
  medianTime <- median(df$totalTimeMin, na.rm = T)
  # TODO: should it be all collected data or only from those params? Right now it is only from params
  # completeSurveys <- attr(df, "numInitial")
  completeSurveys <- nrow(df)
  # Create table -------
  out <- tibble(Campaign,
                SurveyID,
                Stage = stage,
                Medium = medium,
                Source = source,
                `Number of questions` = NQuestions,
                `Time to completion (median -20min+)` = medianTime,
                `Completed surveys` = completeSurveys
  )
  return(out)
}
