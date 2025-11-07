#' CLT Binomial
#'
#' @param n the sample size
#' @param iter the number of iterations
#' @param p the probability
#' @param ... any other parameters
#'
#' @returns a histogram of a binomial distribution with the density and theoretical
#' @export
#'
#' @examples
#' \dontrun mycltb1(n= 10, iter= 10000, p= 0.7)
mycltb1=function(n,iter,p=0.5,...){

  ## r-random sample from the Binomial
  y = rbinom(n*iter,size=n,prob=p)
  ## Place these numbers into a matrix
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  w = apply(data,2,mean)
  ## We will make a histogram of the values in w
  param=hist(w,plot=FALSE)
  ## Since the histogram will be a density plot we will find the max density
  ymax=max(param$density)
  ## To be on the safe side we will add 10% more to this
  ymax=1.1*ymax
  ## Now we can make the histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)
  ## add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3)
  ## Add a theoretical normal curve
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)

}
