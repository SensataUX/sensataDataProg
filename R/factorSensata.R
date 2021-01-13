#' Function to create factors from sensata data
#'
#' This function allows you to create a dictionary from Sensata's data exported from Mongo.
#' @param dat datafile downloaded from Mongo
#' @param exclude question (ex. q2) to be excluded from the dictionary (ex because is an exit screen)
#' @param f file path to save the dictionaries
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return Dataframe dictionary and exported to .csv and .Rdata useful for data cleaning
#' @keywords dictionary diccionario sensata
#' @import tidyverse
#'
#' @examples
#' diccFunction("IND_data.csv", q2)
#' @export


# factorSensata
