#' Function to create sensata dictionary
#'
#' This function allows you to create a dictionary from Sensata's data exported from Mongo.
#' @param data data downloaded from Mongo and loaded to R
#' @param params columns to create on the dictionary. 
#' @param expandOptions logical. If FALSE, the dictionary will have one row per question.
#'
#' @author Camilo Delvasto \email{camilo@@sensata.io} & Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' dictGenerator(data = demoData, params = params, expandOptions = F)
#' @export

# Generates a dictionary with a table structure using a dataset
# Uses the current newResponses.q1.identifier structure
# TODO: the following line is redundant in all but one iteration: acc['qid'] <- b[1]
# TODO: allow ',' inside response options

dictGenerator <- function(data, params = c('identifier', 'question', 'type', 'options'), expandOptions = T){

  # Use the following packages
  require(tidyverse) 
  
  # Fill up and reduce to one row of data
  data <- data %>% select(matches("newResponses"))
  data <- data %>% fill( everything(), .direction="downup")
  data <- data[1,]
  
  # Decide which columns to keep on the dictionary and loop through the data to populate the rows
  acc <- list()

  for(param in params) {
    a <- data %>% select(ends_with(param))
    a <- rename_with(a, ~ gsub(paste('.', param, sep = ''), "",.x, fixed = T))
    a <- a %>% pivot_longer(cols = starts_with('newResponses'),
                            names_to = 'qid',
                            names_prefix='newResponses.',
                            values_to = param)
    acc['qid'] <- a[1]
    acc[param] <- a[2]
    rm(a)
  }
  output <- as.data.frame(do.call(cbind, acc)) 

  # Add the options as a new column (this will transform from the ["1", "2, ...] notation to something R can use)
  for(qid in output['qid']) {
    temp <- gsub('\\[|\\]|\"', '', as.character(data[paste('newResponses.', qid, '.options', sep = '')]))
    output[which(qid == output['qid']), 'options'] <- temp
    rm(temp)
  }
  
  # Duplicate rows as response options are found
  # For now create options of screen and sliders
  # TODO: Add options for traditionalQuestion question type
  output$options[output$options == ""] <- '-'
  output$options[output$options == "[]"] <- '-'
  

  if(expandOptions){
    output <- splitstackshape::cSplit(output, "options", sep = ",", direction = "long") %>% as_tibble()
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
