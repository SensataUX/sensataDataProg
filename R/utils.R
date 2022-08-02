#' Divide by a thousand (usually to convert milliseconds to seconds)
#'
#' This function divides any vector by a thousand, we use it regularly to convert seconds to milliseconds
#' @param x vector to divide by a thousand (ex. from milliseconds to seconds)
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return vector to be divided by a thousand
#' @keywords divide sensata seconds segundos thousands milliseconds
#'
#'
#' @examples
#' dividethou_fun(data$q1time)
#' @export


dividethou_fun <- function(x) {
  r <- x/1000
  r
}

#' Divide by a hundred (usually to change percentage to proportion)
#'
#' This function divides any vector by a hundred, we use it regularly to convert percentage to proportion
#' @param x vector to divide by a hundred (ex. from percentage to proportion)
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return vector to be divided by a hundred
#' @keywords divide sensata hundreds percentage proportion
#'
#'
#' @examples
#' dividethou_fun(data$percentage)
#' @export


dividehun_fun <- function(x) {
  r <- x/100
  r
}


#' Selecter for columns that are ALL NA
#'
#' This a selection helper is used normally inside a select call to drop variables that are all NA
#' @param df
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return selects all variables where ALL rows are NA
#' @keywords sensata selecter na
#'
#'
#' @examples
#' select(where(not_all_na))
#' @export


not_all_na <- function(x) any(!is.na(x))

#' Selecter for columns that have ANY NA
#'
#' This a selection helper is used normally inside a select call to drop variables that are all NA
#' @param df
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return selects all variables where ALL rows are NA
#' @keywords sensata selecter na
#'
#'
#' @examples
#' select(where(not_any_na))
#' @export


not_any_na <- function(x) all(!is.na(x))


#' Function to rescale a vector to 0-100 scale
#'
#' This function takes any vector and rescales it in a linear way so that the new range is 0 to 100. Useful specially when creating indicators
#' @param x numeric vector to rescale to 0-100
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return numeric vector in a 0-100 scale
#' @keywords rescale sensata indicators
#'
#'
#' @examples
#' x <- c(1,2,3,1,3,2,4,5,4,3,2,5,4,3,2,1,4,5)
#' from0to100(x)
#' @export


from0to100 <- function(x){
  if(max(x, na.rm = T) == 1 && min(x, na.rm = T) == 0){
    o = x*100
  } else {
    o = (1-((max(x, na.rm = T)-x)/(max(x, na.rm = T)-1)))*100
  }
  o
}

#' Function to eliminate extra characters for conjoint
#'
#' This function eliminates extra characters, usually used for conjoint. For now it only eliminates the characters from the conjoint options field
#' @param x conjoint options column (or character vector)
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return character vector without extra characters
#' @keywords clean characters sensata conjoint
#'
#'
#' @examples
#' delExtraCar(cjData$conjoint.0.options)
#' @export


delExtraCar <- function(x) {
  o <- x %>%
    str_replace_all('","', "     ") %>%
    str_replace_all("[[:punct:]]", "") %>%
    str_replace_all("brbrbrbr", "__") %>%
    str_replace_all("brbrbr", "__") %>%
    str_replace_all("brbr", "__") %>%
    str_replace_all("     ", " / ")
  o
}

