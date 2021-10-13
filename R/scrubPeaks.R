# Scrub atypical peaks .R
# Description: Function to scrub atypical peaks, to control manipulation attempts
# Created by: Gabriel N. Camargo-Toledo
# Created on: Aug/10/2021
# Modified by: Gabriel N. Camargo-Toledo
# Modified on: Aug/10/2021
# Contact: gcamargo@sensata.io
# Sensata Asus VivoBook PopOs! 21.04 8gb Ram R4.0.4
# Requires: tidyverse, lubridate
# Input: microdata after cleanCols and scrubRows
# Function: scrubpeaks.R
# Output: microdata without rows from atypical peaks and attributes saving number of individuals



# scrubPeaks ---------------------------------------------------------------

#' Function to scrub atypical peaks to control manipulation attempts
#'
#' This function creates the microdata after supressing atypical peaks
#' @param df microdata after cleanCols and scrubRows
#' @param iniPico datetime beginning of peak ex. "2021-08-06 07:00:00", default: NULL if you need to drop all surveys from una medium.
#' @param finPico datetime beginning of peak ex. "2021-08-09 07:00:00", default: NULL if you need to drop all surveys from una medium.
#' @param mediumToScrub medium value to scrub ex. "referral", default: NULL if you need to drop all surveys from a date range
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return microdata without rows from atypical peaks and attributes saving number of individuals
#' @keywords sensata microdata metadata data-cleaning CCV21-1
#' @import tidyverse, lubridate
#'
#' @export
#'

scrubPeaks <- function(df,
                           iniPico = NULL,
                           finPico = NULL,
                           mediumToScrub = NULL){

  rlang::inform("Your Sys.timezone() must be Bogotá for this function to work")
  rlang::inform(paste0("Your Sys.timezone is: ", Sys.timezone()))
  df$createdAt <- df$createdAt %>% lubridate::with_tz(Sys.timezone())

  iniPico <- lubridate::as_datetime(iniPico, tz = Sys.timezone())
  finPico <- lubridate::as_datetime(finPico, tz = Sys.timezone())

  if (is.null(iniPico) && is.null(finPico) && is.null(mediumToScrub)) {
    rlang::abort("You didn't provide any dates or medium to scrub, so there is no peak to scrub.")
  }
  if (is.null(iniPico) && !is.null(finPico)){
    rlang::abort("You didn't provide a start of peak but you provided an end. This makes no sense")
  }
  if (!is.null(iniPico) && is.null(finPico) && !is.null(mediumToScrub)){
    rlang::warn(paste0("You provided only the start of the peak, I will assume that you meant everything after ", iniPico, ", from medium", mediumToScrub))
    df <- df %>% filter(createdAt >= iniPico & params.utm_medium != mediumToScrub)
  }
  if (!is.null(iniPico) && is.null(finPico) && is.null(mediumToScrub)){
    rlang::warn(paste0("You provided only the start of the peak and no medium, I will assume that you meant everything after ", iniPico, ", from all mediums"))
    df <- df %>% filter(createdAt >= iniPico)
  }
  if (is.null(iniPico) && is.null(finPico) && !is.null(mediumToScrub)){
    rlang::warn(paste0("You provided only medium, I will scrub all rows from medium ", mediumToScrub))
    df <- df %>% filter(params.utm_medium != mediumToScrub)
  }
  if (!is.null(iniPico) && !is.null(finPico) && !is.null(mediumToScrub)){
    rlang::inform(paste0("Peak starting at:",iniPico,", ending at ", finPico, " from medium ", mediumToScrub ))
    df <- df %>% filter(!((createdAt >= iniPico & createdAt <= finPico) & params.utm_medium == mediumToScrub))
  }
  # saving attributes
  attr(df, "numAfterPeak") <- nrow(df)
  attr(df, "numFinal") <- nrow(df)
  #output
  df
}
