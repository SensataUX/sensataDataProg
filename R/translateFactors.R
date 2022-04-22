# translateFactors V1.0.1
# Created by: Gabriel N. Camargo-Toledo
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Apr/21/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.04 8gb Ram R4.1.1
# TODO: Translate multiple option questions, possible solution on cuidado/scripts/cleaningData.R
# TODO: Find out why in multiple option questions the translation has different length.

# translateFactors --------------------------------------------------------------

#' Function to translate factors from sensata data two different languages
#'
#' This function allows you to create factors with the information from the dictionary to the variables in the microdata.
#' DO NOT USE for NPSor slider questions that have range different than 0-10.
#' @param df data downloaded from Mongo, cleaned with cleanData.R and scrubbed with scrubData.R
#' @param fromDictionary dictionary created using dictGenerator.R in expanded form without screens in the original language
#' @param toDictionary dictionary created using dictGenerator.R in expanded form without screens in the target language

#' @param questionPrefix Character that identifies questions
#' @param skipQuestionString value that represents the skipped questions. By default "Saltar pregunta"
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with questions in format that is analysis friendly.
#' @keywords sensata microdata data-cleaning factors dictionary
#' @import tidyverse
#' @import labelled
#'
#' @examples
#' TBD
#' @export

translateFactors <- function(
  df,
  fromDictionary,
  toDictionary,
  questionPrefix = "q_",
  skipQuestionString = "Saltar pregunta"
) {


  # Message about skip question string and options orders
  rlang::inform("This function assumes that the options of questions are in the same order and have the same amount of options in the dictionaries. If not, you have to change the dictionaries. It also assumes that the identifiers are identical in the dictionaries.")

  # Saving data attributes of sample size (number of rows) for report ----------
  at <- attributes(df)
  at <- at[grep("num", names(at))]

  # Change skipped questions, false and true --------------------------------------------
  createdAt <- df["createdAt"]
  df["createdAt"] <- NA

  df[df == "true"] <- "1"
  df[df == "false"] <- "0"
  df[df == "NS-NR"] <- skipQuestionString
  df[df == "S99"] <- skipQuestionString
  df["createdAt"] <- createdAt

  # Vector of only questions ------------------------------------------------
  screensVec <- fromDictionary %>% filter(type == "screen")
  screensVec <- screensVec$identifier
  questionsVec <- df %>% select(contains(questionPrefix), -all_of(screensVec)) %>% names()

  # Loop over questions -----------------------------------------------------
  for(v in questionsVec){
    #Creating dictionaries only for this variable
    dictFrom <- filter(fromDictionary, identifier == v)
    dictTo <- filter(toDictionary, identifier == v)
    rlang::inform(paste0("From: ",dictFrom$identifier))
    rlang::inform(paste0("To: ",dictTo$identifier))

    #Expanding dictionary if qid are all unique
    if(any(!(duplicated(dictFrom[["qid"]])))){
      dictFrom <- dictFrom %>% separate_rows(options, sep = "_", convert = T)
    }

    if(any(!(duplicated(dictTo[["qid"]])))){
      dictTo <- dictTo %>% separate_rows(options, sep = "_", convert = T)
    }

    #force options as character
    dictFrom[["options"]] <- as.character(dictFrom[["options"]])
    dictTo[["options"]] <- as.character(dictTo[["options"]])

    #Creating indicator if multiple
    isMultiple <- if_else(dictTo[["maxResponses"]][1] > 1, TRUE, FALSE)
    isSorting <- dictTo[["isSorting"]][1]

    #Creating indicator if close question
    isClose <- if_else(dictTo[["options"]][1] == "-" & dictTo[["numberOfOptions"]][1] == 0, FALSE, TRUE)

    #Number of options
    if(is.na(dictFrom[["altOption"]][1])){
      nOptions <- 1:dictFrom[["numberOfOptions"]][1]
    } else {
      nOptions <- 1:(as.numeric(dictFrom[["numberOfOptions"]][1])+1)
    }



    # Creating levels and labels of factors and columns for multiple choice -------------
    # Single choice questions ------
    if(!(isMultiple) && isClose){

      lab <- c(dictTo[["options"]], skipQuestionString)
      lev <- c(dictFrom[["options"]], skipQuestionString)

      # Modifying labels and levels for ordered, NPS and slider
      if(dictTo[["isOrdered"]][1]){
        # lev <- c(1:(dictFrom[["numberOfOptions"]][1]), skipQuestionString)
      }
      if(dictTo[["type"]][1] == "NPS"){
        lev <- c(0:10, skipQuestionString)
        lab <- c(dictTo[["options"]], skipQuestionString)
      }
      if(dictTo[["type"]][1] == "slider"){
        rlang::inform(paste0(v, " is slider, you need to check if the data was translated correctly."))
        # lev <- c(0:10, skipQuestionString)
        # lab <- c(dict[["options"]], skipQuestionString)
        lab <- levels(factor(df[[v]]))
        lev <- levels(factor(df[[v]]))
      }
      #Adding emoji labels
      #TODO: Add emojis labels argument
      if(dictTo[["type"]][1] == "emojiBubbles"){
        rlang::inform(paste0(v, " is emojiBubbles, the labels will be: crying, sad, neutral, smiling, very happy. If other emojis, need to fix"))
        lab <- c("Crying", "Sad", "Neutral", "Smiling", "Very happy", skipQuestionString)
        lev <- c("Crying", "Sad", "Neutral", "Smiling", "Very happy", skipQuestionString)
      }
      #Adding altOption to ordered vars
      if("altOption" %in% names(dictTo) && !is.na(dictTo[["altOption"]][1]) && dictTo[["isOrdered"]][1]){
        lab <- c(dictTo[["options"]], dictTo[["altOption"]][1], skipQuestionString)
        lev <- c(1:(c(nOptions,(max(nOptions+1)))))
      }
      #Adding altOption to non-ordered vars
      if("altOption" %in% names(dictTo) && !is.na(dictTo[["altOption"]][1]) && !(dictTo[["isOrdered"]][1])){
        lab <- c(dictTo[["options"]], dictTo[["altOption"]][1], skipQuestionString)
        lev <- c(dictTo[["options"]], dictTo[["altOption"]][1], skipQuestionString)
      }
    }

    # Multiple choice questions ------
    if(isMultiple && !isSorting){
      rlang::inform(paste0(v," is multiple choice, check that translation is correct"))
      for(i in c(1:dictFrom$numberOfOptions[1])){
        df[[v]] <- df[[v]] %>% str_replace(pattern = dictFrom[["options"]][i], replacement = dictTo[["options"]][i])
      }
      intoVec <- paste0("MUL", nOptions)
      df <- df %>% separate(col = v,
                            into = c(intoVec),
                            sep = "/",
                            remove = F,
                            fill = "right")

      optionsToVec <- dictTo[["options"]]
      optionsFromVec <- dictFrom[["options"]]

      for(o in optionsToVec){
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
    if(isMultiple && isSorting){
      rlang::inform(paste0(v," is multiple choice, check that translation is correct"))
      for(i in c(1:dictFrom$numberOfOptions[1])){
        df[[v]] <- df[[v]] %>% str_replace(pattern = dictFrom[["options"]][i], replacement = dictTo[["options"]][i])
      }
      intoVec <- paste0(v, "_position_", nOptions)
      df <- df %>% separate(col = v,
                            into = c(intoVec),
                            sep = "/",
                            remove = F,
                            fill = "right")
    }

    # Force Ordered questions -----
    if(dictTo[["isForceOrdered"]][1] && !is.na(dictTo[["isForceOrdered"]][1])){
      # lev <- levels(df[[v]])
      # lab <- levels(df[[v]])
      dictTo[["isOrdered"]] <- T
    }
    if(!(isMultiple) && isClose){
      df[[v]] <- factor(df[[v]],
                        levels = lev,
                        labels = lab,
                        ordered = as.logical(dictTo[["isOrdered"]][1]))
    }
  }
  # Label vars --------------------------------------------------------------
  labList <- toDictionary %>%
    filter(type!="screen") %>%
    filter(identifier %in% fromDictionary[["identifier"]]) %>%
    select("identifier", "question") %>%
    unique() %>%
    pivot_wider(names_from = identifier, values_from = question) %>%
    as.list()

  tryCatch(
    error = function(cnd) rlang::inform("Labels not of the same length, check what happened"),
    var_label(df) <- labList
    )

  # Recovering attributes for report ---------------------------------
  attributes(df) <- c(attributes(df), at)

  return(df)
}

