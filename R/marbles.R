#make a function that allows you to make as many iterations of marble pulling
#' Marble sampling iterations
#'
#' @param iter #iterations
#' @param size #n
#' @param white #12 marbles
#' @param black #8 marbles
#'
#' @returns #the iterations of that sample
#' @export
#'
#' @examples #sample_marbles(iter=4)
sample_marbles <- function(iter = 1, size = 5, white = 12, black = 8) {
  # Create the marble vector
  marbles <- c(rep(1, white), rep(0, black))

  # Repeat sampling for the specified number of iterations
  results <- replicate(
    n = iter,
    expr = sample(marbles, size = size, replace = FALSE),
    simplify = FALSE
  )

  return(results)
}

#sample_marbles(iter= 4)
