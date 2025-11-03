#' ntickets
#'
#' @param N  the number of available seats on the plane
#' @param p  the probability that a passenger will show
#' @param gamma  the probability that the flight will be overbooked
#'
#' @importFrom stats pbinom pnorm
#' @importFrom graphics abline barplot points
#'
#' @returns  The plots of the descrete and continuous distributions, list of values
#' @export
#'
#' @examples
#' \dontrun{ntickets(N= 400, p = 0.95, gamma = 0.02)}
ntickets <- function(N, p, gamma) {
  #First I'm going to define the range for n, which will be important to finding n
  n_range <- seq(N, N + 50, by = 1)

  # Objective functions
  #Using one of the quizzes and the slides, I made the discrete function
  obj<- 1 - gamma - pbinom(N, size = n_range, p)
  obj_d <- abs(obj)


  #for the normal function, we need to find the mean and standard deviation as
  #those are for the z score which will be placed into the pnorm function
  z <- (N + 0.5 - n_range *p) / sqrt(n_range *p * (1-p))
  approx <- pnorm(z)
  obj_1 <- 1 - gamma - approx
  obj_c <- abs(obj_1)

  # index the minimum values of n
  indx_d <- which.min(obj_d)
  indx_c <- which.min(obj_c)

  # Find minimizing n values
  nd <- n_range[indx_d]
  nc <- n_range[indx_c]

  # Plots
  # Plot the discrete function
  plot(n_range, obj, type = "l", lty = "dotdash", col = "black", ylab = "Objective Function Value", xlab = "Number of Tickets (n)",
       main = "Objective Function vs. n with Discrete")
  #adds points to the graph
  points(n_range, obj, pch = 20, col = "black", bg = "navy", cex= 0.5)
  #marks the best number of n
  points(nd, obj[indx_d], pch = 20, col = "green", cex = 1.5)
  # add intersecting lines
  abline(h = 0, col = "red", lty = 1)
  abline(v= n_range[indx_d], col = "red", lty =1)

  #Plot the continuous
  plot(n_range, obj_1, type = "l", col = "black", ylab = "Objective Function Value", xlab = "Number of Tickets (n)",
       main = "Objective Function vs. n Continuous")
  #marks the best number of n
  points(nd, obj[indx_d], pch = 20, col = "green", cex = 1.5)
  # add intersecting lines
  abline(h = 0, col = "red", lty = 1)
  abline(v= n_range[indx_c], col = "red", lty =1)

  # Return named list
  result <- list(
    nd = nd,
    nc = nc,
    N = N,
    p = p,
    gamma = gamma
  )

  print(result)
}
