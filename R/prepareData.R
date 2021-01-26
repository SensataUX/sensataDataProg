# prepareData.R
# Description: Set of functions to prepare sensata microdata for analysis and reports. 
# Created by: Gabriel N. Camargo-Toledo
# Created on: Jan/19/2021			
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Jan/26/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Windows 10 8gb Ram R4.0.3
# Requires: tidyverse, labelled
# Input: data from sensata platform using contentful+mongoDb. Latest data architecture of newResponses. 
# Input: Dictionary created using dictGenerator.R
# Functions: cleanCols, scrubRows, makeFactors, selectCols
# Functions description: cleanCols operates on columns, scrubRows on rows and arrangeCells on cells.
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
                                                 "fingerprint", 
                                                 "browserReport.ip",
                                                 "sensataId",
                                                 "createdAt"),
                      removeParams = FALSE,
                      removeScreens = TRUE){
  
  # Load packages
  require(tidyverse)
  require(labelled)
  
  # Rename _id
  df <- df %>% rename(id = `_id`)
  
  
  # Select columns --------------------------------
  df <- df %>% select(all_of(colsToKeep),
                          starts_with("params"),
                          ends_with(".selected"), 
                          ends_with(".timeToCompletion"))
  if (removeParams) {
    df <- df %>% select(-starts_with('params'))
  }
  
  # Rename columns --------------------------------------------------------
  df <- df %>% rename_with( ~ gsub(".timeToCompletion", "_time",.x, fixed = T))
  df <- df %>% rename_with( ~ gsub("newResponses.", "",.x, fixed = T))
  df <- df %>% rename_with( ~ gsub(".selected", "",.x, fixed = T))
  
  # rename as identifier codes according to dictionary
  codList <- dictionary %>% 
    select("qid", "identifier") %>% 
    unique() %>%
    pivot_wider(names_from = identifier, values_from = qid) %>%
    as.list()
  df <- df %>% rename(!!! codList)
  
  rm(codList)
  
  # Remove screens ---------------------------------------------------
  if(removeScreens){
    screensVec <- dictionary[["identifier"]][dictionary[["type"]]=="screen"]
    df <- df %>% select(-(all_of(screensVec)))
  }

  # output ------------------------------------------------------------------
  return(df)
  
}


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
#' @param ageVal value of age that should be excluded
#' @param testParamName character object of name of test param, usually test
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with the cases scrubbed, and the attributes with the number of cases left after each step for the report: oriNum, removeDupesNum, ageNum, timeNum, geoNum, and finNum
#' @keywords sensata microdata metadata data-cleaning data-scrubbing
#' @import tidyverse
#'
#' @examples
#' bogData1 <- bogData1 %>% scrubData(removeDupes = F, ageVar = "EVCS2", ageVal = "Menos de 18 años", geoloc = T)
#' @export

# TODO: How to include another variable or vector of variables.
# TODO: Geolocation fencing. 


# REV: this function should also be used to remove row that do not comply with other criteria (user did not give consent, etc). Those options should be included here somehow
# RES: this can be very project specific, not only in the variables used to determine the criteria but the value of the criteria. Not sure if it is worth the effort, or just write a small script for those peculiar cases.
scrubRows <- function(df, 
                      removeDupes = T, 
                      timeMin = 2.5, 
                      geoloc = F, 
                      ageVar = NULL, 
                      ageVal,
                      testParamName = NULL){
  # Load packages
  require(tidyverse)
  

  # Erase test --------------------------------------------------------------
  # if(!is.null(testParamName)){
  #   paramName <- paste0("params.", testParamName)
  #   df <- df %>% filter(paramName!=TRUE)
  # }
  
  # Add attribute original number of rows for report --------------------------------
  numInitial <- nrow(df)
  
  # Duplicates --------------------------------------------------------------
  if(removeDupes){  
    df["Dups"] <- duplicated(df["sensataId"])
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
    df <- df %>% filter(!(is.na(geolocation.coordinates)))
    numAfterGeo <- nrow(df)
  } else {
    numAfterGeo <- "NA"
  }
  
  # Age ---------------------------------------------------------------------
  if(!is.null(ageVar)){
    ageVar <- enquo(ageVar)
    ageVal <- enquo(ageVal)
    df <- df %>% filter(!!ageVar != !!ageVal)
    numAfterAge <- nrow(df)
  } else {
    numAfterAge <- "NA"
  }
  
  # Save final number of people -------------------------------------------
  numFinal <- nrow(df)
  
  # Add number of rows as attribute -----------------------------------------
  attr(df, "numInitial") <- numInitial
  attr(df, "numAfterDupes") <- numAfterDupes
  attr(df, "numAfterTime") <- numAfterTime
  attr(df, "numAfterGeo") <- numAfterGeo
  attr(df, "numAfterAge") <- numAfterAge
  attr(df, "numFinal") <- numFinal
  
  # output ------------------------------------------------------------------
  return(df)
}

# makeFactors --------------------------------------------------------------

#' Function to create factors from sensata data
#'
#' This function allows you to create factors with the information from the dictionary to the variables in the microdata. 
#' DO NOT USE for NPSor slider questions that have range different than 0-10. 
#' @param df data downloaded from Mongo, cleaned with cleanData.R and scrubbed with scrubData.R
#' @param dictionary dictionary created using dictGenerator.R in expanded form
#' @param questionPrefix Character that identifies questions
#' @param skipQuestionString value that represents the skipped questions. By default "Saltar pregunta"
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with questions in format that is analysis friendly.
#' @keywords sensata microdata data-cleaning factors dictionary
#' @import tidyverse
#'
#' @examples
#' coronaTestData <-makeFactors(df = bogData3, dict = bogExpDic)
#' @export

makeFactors <- function(
  df, 
  dictionary, 
  questionPrefix = "q_",
  skipQuestionString = "Saltar pregunta"
) {
  # Loading packages
  require(tidyverse)
  
  # Saving data attributes of sample size (number of rows) for report ----------
  at <- attributes(df)
  at <- at[grep("num", names(at))]
  
  # Change skipped questions, false and true --------------------------------------------
  df["createdAt"] <- as.character(df["createdAt"])
  
  df[df == "true"] <- "1"
  df[df == "false"] <- "0"
  df[df == "NS-NR"] <- skipQuestionString
  df[df == "S99"] <- skipQuestionString
  
  # Vector of only questions ------------------------------------------------
  questionsVec <- df %>% select(contains(questionPrefix)) %>% names()
  
  # Loop over questions -----------------------------------------------------
  for(v in questionsVec){
    #Creating dictionary only for this variable
    dict <- filter(dictionary, identifier == v)
    
    #Creating indicator if multiple
    isMultiple <- if_else(dict[["maxResponses"]][1] > 1, TRUE, FALSE)
    isSorting <- dict[["isSorting"]][1]
    #Number of options
    nOptions <- 1:dict[["numberOfOptions"]][1]
    
    # Creating levels and labels of factors and columns for multiple choice -------------
    # Single choice questions ------
    if(!isMultiple){
      
      lab <- c(levels(factor(dict[["options"]])), skipQuestionString)
      lev <- c(levels(factor(dict[["options"]])), skipQuestionString)
      
      # Modifying labels and levels for ordered, NPS and slider
      if(dict[["isOrdered"]][1]){
        lev <- c(1:(dict[["numberOfOptions"]][1]), skipQuestionString)
      }
      if(dict[["type"]][1] == "NPS"){
        lev <- c(0:10, skipQuestionString)
        lab <- c(as.character(factor(dict[["options"]])), skipQuestionString)}
      if(dict[["type"]][1] == "slider"){
        lev <- c(0:10, skipQuestionString)
        lab <- c(as.character(factor(dict[["options"]])), skipQuestionString)}
    }
    
    
    # Multiple choice questions ------
    if(isMultiple & !isSorting){
      intoVec <- paste0("MUL", nOptions)
      df <- df %>% separate(col = v, 
                            into = c(intoVec),
                            sep = "/", 
                            remove = F, 
                            fill = "right")
      
      optionsVec <- as.character(dict[["options"]])
      for(o in optionsVec){
        colName <- paste0(v,"_",o) %>%
          str_replace_all(" ", "_") %>%
          str_replace_all("[^a-zA-Z0-9_]", "") %>%
          str_trunc(30, ellipsis = "")
        df[[colName]] <- if_else(str_detect(df[[v]], o), 1, 0)
        df <- df %>% relocate(all_of(colName), .after = all_of(v))
      }
      
      df <- df %>% select(!starts_with("MUL"))
    }
    
    # Sorting questions -----
    if(isMultiple & isSorting){
      intoVec <- paste0(v, "_position_", nOptions)
      df <- df %>% separate(col = v, 
                            into = c(intoVec),
                            sep = "/", 
                            remove = F,
                            fill = "right")
    } 
    if(!isMultiple){
      df[[v]] <- factor(df[[v]],
                        levels = lev,
                        labels = lab,
                        ordered = as.logical(dict[["isOrdered"]][1]))
    }
  }
  # Label vars --------------------------------------------------------------
  labList <- dictionary %>% 
    filter(type!="screen") %>%
    select("identifier", "question") %>% 
    unique() %>%
    pivot_wider(names_from = identifier, values_from = question) %>%
    as.list()
  
  var_label(df) <- labList
  
  # Recovering attributes for report ---------------------------------
  attributes(df) <- c(attributes(df), at)
  
  return(df)
}

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
                       dropParams = T,
                       dropUserData = T,
                       dropMetaData = T,
                       dropTotalTime = T, 
                       dropQuestionTime = T){
  require(tidyverse)
  dropVec <- vector()
  if(dropGeo){
    dropVec <- c("geolocation.coordinates")
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



