# scrubRows.R V1.1
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

# scrubRows ---------------------------------------------------------------

#' Function to scrub sensata data
#'
#' This function eliminates cases because they are duplicates, or minors, or due to duration of the survey, or because it doesn't have geolocation coordinates.
#' It also creates attributes with the number of people left after each scrubbing step.
#' @param df data downloaded from Mongo and cleaned with cleanData.R
#' @param removeDupes logical, if TRUE it scrubs removeDupeslicate data
#' @param timeMin minimum amount of minutes that the survey should have. Default 2.5 mins. If no scrubbing by time is required make it 0.
#' @param geoloc logical, if TRUE it will scrub surveys that have no geolocation.
#' @param ageVar name of variable of age variable, if empty then it will not scrub by age.
#' @param ageVal value(s) of age that should be excluded, if ageVar numeric, or more than one ageVar should be scrubbed, provide all values as a vector.
#' @param testParamName character object of name of test param, usually test (the full column is called params.test)
#' @param completeVars character vector of variables that have to be complete. It erases individuals that did not answer ALL of them.
#' @param maxSkippedQs maximum number of missing questions accepted, if someone skips more than this number of questions, then they will be scrubbed.
#' @param skipQuestionString skip question string, by default S99
#' @param particularVal named vector, where name is the variable to be used as filter and the value is the value to be kept
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with the cases scrubbed, and the attributes with the number of cases left after each step for the report: oriNum, removeDupesNum, ageNum, timeNum, geoNum, and finNum
#' @keywords sensata microdata metadata data-cleaning data-scrubbing
#' @import tidyverse
#'
#' @examples
#' bogData1 <- bogData1 %>% scrubData(removeDupes = F, ageVar = "EVCS2", ageVal = "Menos de 18 años", geoloc = T)
#' @export

# TODO: How to include vector of variables.
# TODO: Geolocation fencing.


# REV: this function should also be used to remove row that do not comply with other criteria (user did not give consent, etc). Those options should be included here somehow
# RES: this can be very project specific, not only in the variables used to determine the criteria but the value of the criteria. Not sure if it is worth the effort, or just write a small script for those peculiar cases.
scrubRows <- function(df,
                      removeDupes = T,
                      timeMin = 2.5,
                      geoloc = F,
                      ageVar = NULL,
                      ageVal = NULL,
                      testParamName = NULL,
                      completeVars = NULL,
                      maxSkippedQs = NULL,
                      skipQuestionString = "S99",
                      particularVal = NULL){


  # Erase test --------------------------------------------------------------
  numTest <- 0
  if(!is.null(testParamName)){
    numTest <- nrow(df)
    paramName <- paste0("params.", testParamName)
    df <- df[is.na(df[[paramName]]),]
    numTest <- numTest - nrow(df)
  }

  # Add attribute original number of rows for report --------------------------------
  numInitial <- nrow(df)

  # Duplicates --------------------------------------------------------------
  if(removeDupes){
    allDuplicated <- function(vec){
      front <- duplicated(vec)
      back <- duplicated(vec, fromLast = TRUE)
      all_dup <- front + back > 0
      return(all_dup)
    }
    df["Dups"] <- allDuplicated(df["sensataId"])
    df <- df %>% filter(Dups == F)
    df <- df %>% select(!(Dups))
    numAfterDupes <- nrow(df)
  } else {
    numAfterDupes <- "NA"
  }

  # Time --------------------------------------------------------------------
  df <- df %>% filter(totalTimeMin >= timeMin)
  if(timeMin>0){
    numAfterTime <- nrow(df)
  } else {
    numAfterTime <- "NA"
  }

  # Geolocation -------------------------------------------------------------
  if(geoloc){
    df <- df %>% filter(!(is.na(lat)))
    numAfterGeo <- nrow(df)
  } else {
    numAfterGeo <- "NA"
  }

  # Age ---------------------------------------------------------------------
  if(!is.null(ageVar)){
    # ageVar <- sym(ageVar)
    # ageVal <- enquo(ageVal) {{ageVal}}
    df <- df %>% subset(!(df[[ageVar]] %in% ageVal))
    numAfterAge <- nrow(df)
  } else {
    numAfterAge <- "NA"
  }

  # Complete vars -----------------------------------------------------------
  if(!is.null(completeVars)){
    selectVec <- df[completeVars] == "S99"
    selectVec <- !(apply(selectVec,1, any))
    df <- df[selectVec,]
    selectVec <- is.na(df[completeVars])
    selectVec <- !(apply(selectVec,1, any))
    df <- df[selectVec,]
    numAfterCompleteVars <- nrow(df)
  } else {
    numAfterCompleteVars <- "NA"
  }

  # skippedQs -----------------------------------------------------------
  if(!is.null(maxSkippedQs)){
    df[["perdidos"]] <-  NA
    df[["perdidos"]] <-  apply(df, 1, function(y) sum(length(which(as.character(y) == skipQuestionString))))
    df <- df %>% filter(perdidos<=maxSkippedQs) %>% select(-c("perdidos"))
    numAfterMissing <- nrow(df)
  } else {
    numAfterMissing <- "NA"
  }

  # particularVal -----------------------------------------------------------
  if(!is.null(particularVal)){
    var <- names(particularVal)
    val <- unname(particularVal)
    df <- df %>% subset(df[[var]] %in% val)
    numAfterPartVal <- nrow(df)
  } else {
    numAfterPartVal <- "NA"
  }


  # Save final number of people -------------------------------------------
  numFinal <- nrow(df)

  # Add number of rows as attribute -----------------------------------------
  attr(df, "numTest") <- numTest
  attr(df, "numInitial") <- numInitial
  attr(df, "numAfterDupes") <- numAfterDupes
  attr(df, "numAfterTime") <- numAfterTime
  attr(df, "numAfterGeo") <- numAfterGeo
  attr(df, "numAfterAge") <- numAfterAge
  attr(df, "numAfterCompleteVars") <- numAfterCompleteVars
  attr(df, "numAfterMissing") <- numAfterMissing
  attr(df, "numAfterPartVal") <- numAfterPartVal
  attr(df, "numFinal") <- numFinal

  # output ------------------------------------------------------------------
  return(df)
}

