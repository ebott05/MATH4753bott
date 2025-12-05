#' my_maxlikg
#'
#' @param lfun a function selected, usually a logarithmic function
#' @param theta an input of the mean/certain value
#'
#' @returns a plot of the maximum likelihood of a joint binomial and Poisson density.
#' @export
#'
#' @examples
#' \dontrun{mymaxlikg(theta= seq(1,10, length= 1000))}
my_maxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nr=nth,nc=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}

