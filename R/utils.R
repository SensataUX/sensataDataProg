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


#' Selecter for columns that are ALL NA
#'
#' This a selection helper is used normally inside a select call to drop variables that are all NA
#' @param df
#'
#' @author Gabriel N. Camargo-Toledo \email{gcamargo@@sensata.io}
#' @return selects all variables where ALL rows are NA
#' @keywords divide sensata seconds segundos
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
#' @keywords divide sensata seconds segundos
#'
#'
#' @examples
#' select(where(not_any_na))
#' @export


not_any_na <- function(x) all(!is.na(x))


