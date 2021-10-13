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



