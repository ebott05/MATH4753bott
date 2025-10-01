#' birthday
#'
#' @param x # a vector
#'
#' @returns # probability of a birthday
#' @export
#'
#' @examples # birthday(20:24)
birthday <- function(x){
  + 1 - exp(lchoose(365,x) + lfactorial(x) - x*log(365))
}
birthday(20:24)
