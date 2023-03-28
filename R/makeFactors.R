# makeFactors.R V1
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

# makeFactors --------------------------------------------------------------

#' Function to create factors from sensata data
#'
#' This function allows you to create factors with the information from the dictionary to the variables in the microdata.
#' DO NOT USE for NPSor slider questions that have range different than 0-10.
#' @param df data downloaded from Mongo, cleaned with cleanData.R and scrubbed with scrubData.R
#' @param dictionary dictionary created using dictGenerator.R in expanded form without screens
#' @param questionPrefix Character that identifies questions
#' @param specialSkipValue If you provide an integer (normally 99) it will create labelled_spss (or stata) variables where the skip string has the value provided. Defaults to NULL
#' @param skipQuestionString value that represents the skipped questions. By default "Saltar pregunta"
#' @param multChoiceText A two element vector that determines text of multiple choice questions. By defaul c(1,0). Commonly used ones are c("Yes", "No") or c("Sí", "No"). ORDER MATTERS!
#' @param dummyMultiChoice logic, if TRUE (default), it will create a dummy column for each option on multiple choice questions. Usually turned off when making factors after translation.
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
  skipQuestionString = "Saltar pregunta",
  specialSkipValue = NULL,
  multChoiceText = c(1,0),
  dummyMultiChoice = T
) {


  # Message about skip question string
  rlang::inform("This function assumes that skipped questions are coded as S99 in mongo, if not you need to change the function.")

  # Error if special value is not numeric
  if (!is.numeric(specialSkipValue) && !is.null(specialSkipValue)) {
    rlang::abort("specialSkipValue is not a number, makeFactors will not work")
  }

  # Saving data attributes of sample size (number of rows) for report ----------
  at <- attributes(df)
  at <- at[grep("num", names(at))]
  # Drop from dictionary if numberOfOptions == 0
  dictionary <- dictionary %>% filter(numberOfOptions!="0")

  # Change skipped questions, false and true --------------------------------------------
  createdAt <- df["createdAt"]
  df["createdAt"] <- NA

  df[df == "true"] <- "1"
  df[df == "false"] <- "0"
  df[df == "NS-NR"] <- skipQuestionString
  df[df == "S99"] <- skipQuestionString
  df["createdAt"] <- createdAt

  # Vector of only questions ------------------------------------------------
  questionsVec <- df %>% select(contains(questionPrefix), -ends_with("time")) %>% names()

  # Loop over questions -----------------------------------------------------
  for(v in questionsVec){
    #Creating dictionary only for this variable
    dict <- filter(dictionary, identifier == v)
    print(v)
    #Expanding dictionary if qid are all unique
    if(any(!(duplicated(dict[["qid"]])))){
      dict <- dict %>% separate_rows(options, sep = "_", convert = T)
    }

    #force options as character
    dict[["options"]] <- as.character(dict[["options"]])

    #Creating indicator if multiple
    isMultiple <- if_else(dict[["maxResponses"]][1] > 1, TRUE, FALSE)
    isSorting <- dict[["isSorting"]][1]

    #Creating indicator if close question
    isClose <- if_else(dict[["options"]][1] == "-" & dict[["numberOfOptions"]][1] == 0, FALSE, TRUE)

    #Number of options
    nOptions <- 1:dict[["numberOfOptions"]][1]

    # Creating levels and labels of factors and columns for multiple choice -------------
    # Single choice questions ------
    # If special value provided -----
    if(!(isMultiple) && isClose){
      if(is.null(specialSkipValue)){
        lab <- c(dict[["options"]], skipQuestionString)
        lev <- c(dict[["options"]], skipQuestionString)
      }

      if(is.numeric(specialSkipValue)){
        df[[v]][df[[v]] == skipQuestionString] <- specialSkipValue
        lab <- c(dict[["options"]], skipQuestionString)
        lev <- c(dict[["options"]], specialSkipValue)
      }

      # Modifying labels and levels for ordered, NPS and slider
      if(dict[["isOrdered"]][1]){
        lev <- c(1:(dict[["numberOfOptions"]][1]), skipQuestionString)
      }
      if(dict[["type"]][1] == "NPS"){
        lev <- c(0:10, skipQuestionString)
        lab <- c(dict[["options"]], skipQuestionString)
      }
      if(dict[["type"]][1] == "slider"){
        rlang::inform(paste0(v, " is slider, you need to check if the data was modified correctly."))
        # lev <- c(0:10, skipQuestionString)
        # lab <- c(dict[["options"]], skipQuestionString)
        lab <- levels(factor(df[[v]]))
        lev <- levels(factor(df[[v]]))
      }
      #Adding emoji labels
      if(dict[["type"]][1] == "emojiBubbles"){
        rlang::inform(paste0(v, " is emojiBubbles, the labels will be: crying, sad, neutral, smiling, very happy. If other emojis, need to fix"))
        lab <- c("Crying", "Sad", "Neutral", "Smiling", "Very happy", skipQuestionString)
      }
      #Adding altOption to ordered vars
      if("altOption" %in% names(dict) && !is.na(dict[["altOption"]][1]) && dict[["isOrdered"]][1]){
        lab <- c(dict[["options"]], dict[["altOption"]][1], skipQuestionString)
        lev <- c(1:(dict[["numberOfOptions"]][1]), dict[["altOption"]][1], skipQuestionString)
      }
      #Adding altOption to non-ordered vars
      if("altOption" %in% names(dict) && !is.na(dict[["altOption"]][1]) && !(dict[["isOrdered"]][1])){
        lab <- c(dict[["options"]], dict[["altOption"]][1], skipQuestionString)
        lev <- c(dict[["options"]], dict[["altOption"]][1], skipQuestionString)
      }
    }

    # Multiple choice questions ------
    if(dummyMultiChoice){
      if(isMultiple && !isSorting){
        intoVec <- paste0("MUL", nOptions)
        df <- df %>% separate(col = v,
                              into = c(intoVec),
                              sep = "/",
                              remove = F,
                              fill = "right")

        optionsVec <- dict[["options"]]
        for(o in optionsVec){
          colName <- paste0(v,"_",o) %>%
            str_replace_all(" ", "_") %>%
            str_replace_all("[^a-zA-Z0-9_]", "") %>%
            str_trunc(30, ellipsis = "")
          df[[colName]] <- if_else(str_detect(df[[v]], o), multChoiceText[1], multChoiceText[2])
          df[[colName]] <- df[[colName]] %>% factor()
          if(is.numeric(specialSkipValue)){
            levs <- c(1,2)
            names(levs) <- multChoiceText
            df[[colName]] <- df[[colName]] %>%
              labelled_spss(labels = levs)
          }
          df <- df %>% relocate(all_of(colName), .after = all_of(v))
        }
        df <- df %>% select(!starts_with("MUL"))
      }

      # Sorting questions -----
      if(isMultiple && isSorting){
        intoVec <- paste0(v, "_position_", nOptions)
        df <- df %>% separate(col = v,
                              into = c(intoVec),
                              sep = "/",
                              remove = F,
                              fill = "right")
      }
    }
    if(dict[["isForceOrdered"]][1] && !is.na(dict[["isForceOrdered"]][1])){
      lev <- levels(df[[v]])
      lab <- levels(df[[v]])
      dict[["isOrdered"]] <- T
    }
    # Factor -------
    if(!(isMultiple) && isClose && is.null(specialSkipValue)){
      df[[v]] <- factor(df[[v]],
                        levels = lev,
                        labels = lab,
                        ordered = as.logical(dict[["isOrdered"]][1]))
    }

   # If special value provided -----
    if(!(isMultiple) && isClose && is.numeric(specialSkipValue)){
      # lev <- lev[-length(lev)]
      # lev <- c(1:length(lev), specialSkipValue)
      # lab <- lab[-length(lab)]
      # lab <- c(lab, specialSkipValue)
      # names(lev) <- lab
      # df[[v]] <- df[[v]] %>%
      #   labelled_spss(labels = lev)
    }
  }
  # Label vars --------------------------------------------------------------
  labList <- dictionary %>%
    filter(type!="screen") %>%
    filter(type!="conjoint") %>%
    select("identifier", "question") %>%
    unique() %>%
    pivot_wider(names_from = identifier, values_from = question) %>%
    as.list()

  var_label(df) <- labList

  # fixing id --------------------------------------------------------
  df[["id"]] <- df[["id"]] %>%
    str_remove_all("ObjectId") %>%
    str_remove_all("\\(") %>%
    str_remove_all("\\)")
  # Recovering attributes for report ---------------------------------
  attributes(df) <- c(attributes(df), at)

  return(df)
}

