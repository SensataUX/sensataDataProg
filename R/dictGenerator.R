# DictGenerator V1.0.1
# Created by: Gabriel N. Camargo-Toledo
# Created on: Aug/18/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Apr/01/2022
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook Pop!_OS 21.10 8gb Ram R4.1.2

#' Function to create sensata dictionary
#'
#' This function allows you to create a dictionary from Sensata's data exported from Mongo.
#' @param df data downloaded from Mongo and loaded to R
#' @param cols columns to create on the dictionary.
#' @param expandOptions logical. If TRUE, the dictionary will have one row per answer option. if FALSE, it will be 1 row per question.
#' @param questionPrefix character that identifies questions if not included in contentful. Default "".
#' @param forceOrdered vector of ordered questions that are from a non-ordered types
#' @param responseType which object version should the function use, one of "newResponses" or "structuredResponses"
#'
#' @author Camilo Delvasto \email{camilo@@sensata.io} & Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' dictGenerator(data = demoData, expandOptions = F)
#' @export

# Generates a dictionary with a table structure using a dataset.
# TODO: Fix dictGenerator example

dictGenerator <- function(df,
                          cols = c(#'qid',
                                   'identifier', 'question', 'type', 'options', 'numberOfOptions', 'isOrdered', 'isSorting', 'maxResponses', 'altOption','isRandomGroupChild'),
                          expandOptions = TRUE,
                          questionPrefix = "",
                          forceOrdered = NULL,
                          responseType = "structuredResponses"){

  # warn about answer type if newResponses:
  if(responseType == "newResponses"){
    rlang::inform('_______________________________________________________________________________________
                  You selected "newResponses" responseType , so all questions need to be in the same order for all individuals.
                  If there have been order changes on the questionnaire use "strucResponses" responseType
                  ______________________________________________________________________________________')
  }
  # warn if forceOrdered is NULL:
  if(is.null(forceOrdered)){
    rlang::inform('______________________________________________________________________________________
                  You did not provide a forceOrdered vector, this is usually incorrect,
                  check which questions are ordered among traditional or other types
                  that are not intrinsically ordered.
                  ______________________________________________________________________________________')
  }

  # Fill up and reduce to one row of data
  df <- df %>% select(matches(responseType))
  df <- df %>% fill( everything(), .direction="downup")
  df <- df[1,]

  # Decide which columns to keep on the dictionary and loop through the data to populate the rows
  acc <- list()

  for(c in cols) {
    a <- df %>% select(ends_with(c, ignore.case = F))
    a <- rename_with(a, ~ gsub(paste('.', c, sep = ''), "",.x, fixed = T))
    a <- a %>% pivot_longer(cols = starts_with(responseType),
                            names_to = 'id',
                            names_prefix=responseType,
                            values_to = c)
    acc['id'] <- a[1]
    acc[c] <- a[2]
    rm(a)
  }

  output <- as.data.frame(do.call(cbind, acc))

  # Add the options as a new column (this will transform from the ["1", "2, ...] notation to something R can use)
  for(id in output['id']) {
    temp <- gsub('\\[|\\]', '', as.character(df[paste0(responseType, id, '.options')]))
    temp <- gsub('","', '_', temp)
    temp <- gsub('\"', '', temp)
    output[which(id == output['id']), 'options'] <- temp
    rm(temp)
  }
  tempOutput <- output

  # Duplicate rows as response options are found
  output$options[output$options == ""] <- '-'
  output$options[output$options == "[]"] <- '-'

  output[["identifier"]] <- paste0(questionPrefix, output[["identifier"]])

  # Change type of isOrdered and isSorting to logical
  output[["isSorting"]] <- as.logical(output[["isSorting"]])
  output[["isOrdered"]] <- as.logical(output[["isOrdered"]])

  isForceOrdered <- if_else(condition = is.null(forceOrdered), FALSE, TRUE)
  output[["isForceOrdered"]] <- FALSE
  for(f in forceOrdered){
    output[["isForceOrdered"]][output[["identifier"]] == f] <- isForceOrdered
  }
  if(!is.null(forceOrdered)){
    output[["isForceOrdered"]][is.na(output[["isForceOrdered"]])] <- FALSE
  }
  if(expandOptions){
    #TODO: Included suppressWarnings while splitstackshape updates to R version 4.1.0
    #check this github issue: https://github.com/mrdwab/splitstackshape/issues/71
    output <- suppressWarnings(splitstackshape::cSplit(output, "options", sep = "_", direction = "long") %>% as_tibble())
  }

  # fix characters in identifier
  output[["identifier"]] <- output[["identifier"]] %>% stringi::stri_trans_general(id = "Latin-ASCII")
  # remove tabulation in options
  output$options <- output$options %>% str_remove("\\\\u0009")

  #output <- output %>% rename("order" = "qid")
  output$id <- output$id %>% str_replace(".", "")
  # output <- output %>% filter(!is.na(order))

  # fix bilderset
  output$options[output$type == "bilderset" & str_detect(output$options, "//")] <- output$options[output$type == "bilderset" & str_detect(output$options, "//")] %>% str_extract(".+?(?=//)")
  # fix bilderset
  output$options <- output$options %>% str_trim(side = "both")
  # Output ------------------------------------------------------------------
  return(output)

}

