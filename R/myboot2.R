#' myboot2
#'
#' @param iter the number of iterations
#' @param x the sample
#' @param fun the function to be used
#' @param alpha alpha based on the percent interval
#' @param cx text size for plots
#' @param ... any additional parameters
#'
#' @returns a histogram with the bootstrap interval on it
#' @export
#'
#' @examples
#' \dontrun{myboot2(iter= 10000, x= sam, fun= mean, alpha= 0.05)}
myboot2<-function(iter=10000,x,fun=mean,alpha=0.05,cx=1.5,...){
  n=length(x)

  y=sample(x,n*iter,replace=TRUE) # A
  rs.mat=matrix(y,nr=n,nc=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun)
  ci=quantile(xstat,c(alpha/2,1-alpha/2)) #B
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nr=length(x),nc=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")
  segments(ci[1],0,ci[2],0,lwd=4)
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x))
}
