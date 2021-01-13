#' Function to scrub sensata data
#'
#' This function eliminates cases because they are duplicated, or minors, or other reasons.
#' @param data data downloaded from Mongo and cleaned with cleanData.R
#' @param dup logical, if FALSE it does not scrub duplicate data
#' @param ageVar name of variable of age variable, if empty then it will not scrub by age.
#' @param ageVal value of age that should be excluded
#' @param timeMin minimum amount of minutes that the survey should have. Default 2.5 mins
#' @param geoloc logical, if TRUE it will scrub surveys that have no geolocation.
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary and exported to .csv and .Rdata useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' scrubData("IND_data.csv", q2)
#' @export
 
# scrubData

scrubData <- function(data, dup = T, ageVar, ageVal, timeMin = 2.5, geoloc = F){
  # Loading packages
  require(tidyverse)
  
  # Adding attribute original number of rows --------------------------------
  attr(data, "oriNum") <- nrow(data)

  # Duplicates --------------------------------------------------------------
  if(dup){  
    data["Dups"] <- duplicated(data["sensataId"])
    data <- data %>% filter(Dups == F)
    data <- data %>% select(!(Dups))
    attr(data, "dupNum") <- nrow(data)
  }
  
  # Age ---------------------------------------------------------------------
  if(exists("ageVar")){
    ageVar <- enquo(ageVar)
    ageVal <- enquo(ageVal)
    data <- data %>% filter(!!ageVar != !!ageVal)
    attr(data, "ageNum") <- nrow(data)
  }
  

  # Time --------------------------------------------------------------------
  data <- data %>% filter(totalTimeMin >= timeMin)
  if(timeMin>0){
    attr(data, "timeNum") <- nrow(data)
  }
  
  # Geolocation -------------------------------------------------------------
  if(geoloc){
    data <- data %>% filter(!(is.na(geolocation.coordinates)))
    attr(data, "geoNum") <- nrow(data)
  }

  # Saving final number of people -------------------------------------------
  attr(data, "finNum") <- nrow(data)
  
  # output ------------------------------------------------------------------
  return(data)
}