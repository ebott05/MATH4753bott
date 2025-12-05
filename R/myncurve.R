#create the function to make a normal distribution curve
#' Title
#'
#' @param mu a vector
#' @param sigma a vector
#' @param a av vector
#'
#' @returns a list and plot given mu, sigma, and a and generates probabilities off of a
#' @export
#'
#' @examples
#' \dontrun{myncurve(2, 4, 0.5)}
myncurve = function(mu, sigma, a) {
  # Plot the curve
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu - 3*sigma, mu + 3*sigma), col= "purple"
        , ylab = "Density", main = paste("Normal Curve with mu =", mu, "and sigma =", sigma))

  # find and fill in the probability of x =< a
  xcurve = seq(mu - 3*sigma, a, length=1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu - 3*sigma, xcurve, a), c(0, ycurve, 0), col="purple")

  # Compute probability
  prob = round(pnorm(a, mean=mu, sd=sigma), 4)

  # Return a named list
  list(mu = mu, sigma = sigma, a = a, probability = prob)
}
