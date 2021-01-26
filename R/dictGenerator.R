#' Function to create sensata dictionary
#'
#' This function allows you to create a dictionary from Sensata's data exported from Mongo.
#' @param df data downloaded from Mongo and loaded to R
#' @param cols columns to create on the dictionary. 
#' @param expandOptions logical. If TRUE, the dictionary will have one row per answer option. if FALSE, it will be 1 row per question.
#' @param questionPrefix Character that identifies questions
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
# WARNING: All questions need to be in the same order. If the instrument changes during collection it will break.
# Uses the current newResponses.q1.identifier structure
# TODO: allow ',' inside response options
# TODO: change accents on identifiers
# TODO: add option to erase prefix on identifiers
# TODO: Fix dictGenerator example

dictGenerator <- function(df, 
                          cols = c('identifier', 'question', 'type', 'options', 'numberOfOptions', 'isOrdered', 'isSorting', 'maxResponses', 'altOption'), 
                          expandOptions = TRUE,
                          questionPrefix = "q_"){

  # Use the following packages
  require(tidyverse)
  
  # Fill up and reduce to one row of data
  df <- df %>% select(matches("newResponses"))
  df <- df %>% fill( everything(), .direction="downup")
  df <- df[1,]
  
  # Decide which columns to keep on the dictionary and loop through the data to populate the rows
  acc <- list()

  for(c in cols) {
    a <- df %>% select(ends_with(c, ignore.case = F))
    a <- rename_with(a, ~ gsub(paste('.', c, sep = ''), "",.x, fixed = T))
    a <- a %>% pivot_longer(cols = starts_with('newResponses'),
                            names_to = 'qid',
                            names_prefix='newResponses.',
                            values_to = c)
    acc['qid'] <- a[1]
    acc[c] <- a[2]
    rm(a)
  }
  output <- as.data.frame(do.call(cbind, acc)) 

  # Add the options as a new column (this will transform from the ["1", "2, ...] notation to something R can use)
  for(qid in output['qid']) {
    temp <- gsub('\\[|\\]', '', as.character(df[paste0('newResponses.', qid, '.options')]))
    temp <- gsub('","', '_', temp)
    temp <- gsub('\"', '', temp)
    output[which(qid == output['qid']), 'options'] <- temp
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
  
  if(expandOptions){
    output <- splitstackshape::cSplit(output, "options", sep = "_", direction = "long") %>% as_tibble()
  }
  

  # Output ------------------------------------------------------------------
  return(output)
  
}



# This function requires the following data structure:
#   newResponses.q1.identifier
#   newResponses.q1.type
# This function generates a table like this:
# qid   identifier  question                                    type
# 1       q0        Inicio encuesta de Audiencias               screen
# 2       q1        ¿En qué país resides?                       traditionalQuestion
# 3       q3        ¿Qué mundo prefieres después del COVID-19?  NPS
