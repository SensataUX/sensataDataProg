# extractMetaData.R V0.0.1
# Created by: Gabriel N. Camargo-Toledo
# Created on: Oct/27/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Oct/27/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.1.1
# Requires: tidyverse
# Input: Interim data after scrubRows.R
# Input: Dictionary created using dictGenerator.R
# Output: table for columns A to H (except lang) from file https://docs.google.com/spreadsheets/d/1CdSqvX3Hhayk_NGvr9eqDeAlA8XG9BX_-Exm5K3RKlI/edit#gid=2106088199

#' Extract sensata metadata
#'
#' This function creates table to fill out metadata on https://docs.google.com/spreadsheets/d/1CdSqvX3Hhayk_NGvr9eqDeAlA8XG9BX_-Exm5K3RKlI/edit#gid=2106088199
#' @param df Interim dataset after scrubRows.r
#' @param dict Dictionary created using dictGenerator.R
#' @param stage pilot or live collection for param.utm_campaign
#' @param source wa, fb, email, etc for param.utm_source
#' @param medium cpc, referral, etc for param.utm_medium
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
    df <- df %>% filter(param.utm_source == source)
  }
  if(!is.null(medium)){
    df <- df %>% filter(param.utm_medium == medium)
  }

  # Create columns -------
  Campaign <- tail(df$surveyName, 1)
  NQuestions <- max(as.integer(Dict$order))+1
  medianTime <- median(df$totalTimeMin, na.rm = T)
  completeSurveys <- attr(df, "numInitial")

  # Create table -------
  out <- tibble(Campaign ,
                Stage = stage,
                Medium = medium,
                Source = source,
                `Number of questions` = NQuestions,
                `Time to completion (median -20min+)` = medianTime,
                `Completed surveys` = completeSurveys
  )
  return(out)
}
