# translateFactors --------------------------------------------------------------

#' Function to translate factors from sensata data from one language to another
#'
#' This function allows you to translate factors with the information from the dictionary to the variables in the microdata, from one language to another.
#' DO NOT USE for NPS questions that have range different than 0-10. This function erases attributes about sample sizes.
#' This function requires two dictionaries, one for the original language and another for the language to translate to
#' @param data data downloaded from Mongo, cleaned with cleanData.R and scrubbed with scrubData.R
#' @param fromDict dictionary created using dictGenerator.R from which to translate the data.
#' @param toDic dictionary created using dictGenerator.R to translate the data to. 
#' @param questionPrefix Character that identifies questions
#' @param skipQuestionStringFrom value that represents the skipped questions in original language
#' @param skipQuestionStringTo value that represents the skipped questions in target language
#' 
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe with questions in format that have been prepared with makeFactor
#' @keywords sensata microdata data-cleaning factors dictionary
#' @import tidyverse
#'
#' @examples
#' coronaTestData <-makeFactors(data = bogData3, dict = bogExpDic)
#' @export

translateFactors <- function(
  df, 
  fromDict, 
  toDict,
  questionPrefix = "q_",
  skipQuestionStringFrom,
  skipQuestionStringTo
) {
  # Loading packages
  require(tidyverse)
  
  # Vector of only questions ------------------------------------------------
  questionsVec <- df %>% select(contains(questionPrefix)) %>% names()
  
  # Loop over questions -----------------------------------------------------
  for(v in questionsVec){
    #Creating dictionaries only for this variable
    dict_from <- filter(fromDict, identifier == v)
    dict_to <- filter(toDict, identifier == v)
    
    #Creating indicator if multiple
    isMultiple <- if_else(dict_from[["maxResponses"]][1] > 1, TRUE, FALSE)
    isSorting <- dict_from[["isSorting"]][1]
    #Number of options
    nOptions <- 1:dict_from[["numberOfOptions"]][1]
    
    # Creating levels and labels of factors and columns for multiple choice -------------
    # Single choice questions ------
    if(!isMultiple){
      
      lab <- c(levels(factor(dict_to[["options"]])), skipQuestionStringTo)
      lev <- c(levels(factor(dict_from[["options"]])), skipQuestionStringFrom)
      
      # Modifying labels and levels for ordered, NPS and slider
      if(dict_from[["isOrdered"]][1]){
        lev <- c(1:(dict_from[["numberOfOptions"]][1]), skipQuestionStringFrom)
      }
      if(dict_from[["type"]][1] == "NPS"){
        lev <- c(0:10, skipQuestionStringFrom)
        lab <- c(as.character(factor(dict_to[["options"]])), skipQuestionStringTo)}
      if(dict_from[["type"]][1] == "slider"){
        lev <- c(0:10, skipQuestionStringFrom)
        lab <- c(as.character(factor(dict_to[["options"]])), skipQuestionStringTo)}
    }
    
    # # Multiple choice questions ------
    # if(isMultiple & !isSorting){
    #   intoVec <- paste0("MUL", nOptions)
    #   df <- df %>% separate(col = v, 
    #                         into = c(intoVec),
    #                         sep = "/", 
    #                         remove = F, 
    #                         fill = "right")
    #   
    #   df <- df %>% select(!starts_with(paste0(v, "_")))
    #   
    #   optionsVecTo <- as.character(dict_to[["options"]])
    #   optionsVecFrom <- as.character(dict_to[["options"]])
    #   
    #   for(o in optionsVecTo){
    #     for(f in optionsVecFrom){
    #       colName <- paste0(v,"_",o) %>%
    #         str_replace_all(" ", "_") %>%
    #         str_replace_all("[^a-zA-Z0-9_]", "") %>%
    #         str_trunc(30, ellipsis = "")
    #       df[[colName]] <- if_else(str_detect(df[[v]], a), 1, 0)
    #       df <- df %>% relocate(all_of(colName), .after = all_of(v))
    #     }
    #   }
    #   
    #   df <- df %>% select(!starts_with("MUL"))
    # }
    # 
    # # Sorting questions -----
    # if(isMultiple & isSorting){
    #   intoVec <- paste0(v, "_position_", nOptions)
    #   df <- df %>% separate(col = v, 
    #                         into = c(intoVec),
    #                         sep = "/", 
    #                         remove = F,
    #                         fill = "right")
    # } 
    if(!isMultiple){
      df[[v]] <- factor(df[[v]],
                        levels = lev,
                        labels = lab,
                        ordered = as.logical(dict_from[["isOrdered"]][1]))
    }
  }
  # Label vars --------------------------------------------------------------
  # labList <- toDict %>% 
  #   filter(type!="screen") %>%
  #   select("identifier", "question") %>% 
  #   unique() %>%
  #   pivot_wider(names_from = identifier, values_from = question) %>%
  #   as.list()
  # 
  # var_label(df) <- labList
  
  return(df)
}

