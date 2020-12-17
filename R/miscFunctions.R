#' Miliseconds to seconds (or divide by a thousand)
#'
#' This function divides any vector by a thousand, we use it to convert seconds to milliseconds
#' @param x vector to convert from miliseconds to seconds (or divide by a thousand)
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return vector of seconds or divided by a thousand
#' @keywords divide sensata seconds segundos
#'
#'
#' @examples
#' dividethou_fun(data$q1time)
#' @export


dividethou_fun <- function(x) {
  r <- x/1000
  r
}


#' Percentage of missing values
#'
#' This function counts how many of a vector are missing, and divides them by the number of questions
#' @param x vector to count missings on
#' @param mis character that identifies missing values.
#' @param nques number of questions on the instrument
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return percentage of that vector that is identical to missing value
#' @keywords missing sensata skip
#'
#'
#' @examples
#' dividethou_fun(data$q1time)
#' @export


mis_fun <- function(x, mis="NS-NR", nques) {
  (length(which(x==mis))/nques)*100
}

