#' mycltbin
#'
#' @param n number of times you want to do the trial
#' @param iter your iterations
#' @param p probability
#' @param ...
#'
#' @return a graph
#' @export
#'
#' @examples mycltbin(10,1000,0.6) ;returns a graph
mycltb=function(n,iter,p=0.5,...){

  ## r-random sample from the Binomial
  y=rbinom(n*iter,size=n,prob=p)
  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)
  ## apply the function mean to the columns (2) of the matrix
  ## these are placed in a vector w
  w=apply(data,2,mean)
  ## We will make a histogram of the values in w
  ## How high should we make y axis?
  ## All the values used to make a histogram are placed in param (nothing is plotted yet)
  param=hist(w,plot=FALSE)

  ymax=max(param$density)
  ymax=1.1*ymax

  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)
  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)
}
