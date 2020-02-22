#' myNCurve
#'
#' @param mu your mean
#' @param sigma your standard deviation
#' @param a where you wanna stop your prob. estimate; P(-7<Y<a)
#'
#' @return a graph that shows the probability
#' @export
#'
#' @examples myNCurve(10,5,9); returns a graph
myncurve = function(mu, sigma, a){

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve1 = seq(-7,a,1000)
  ycurve1 = dnorm(xcurve1, 0,mu, sigma)
  polygon(c(-7,xcurve1,a), c (0,ycurve1,0), col = "midnightblue")
  area = pnorm(a, mu,sigma) - pnorm(-5,mu,sigma)
  area = round(area,4)
  text((-7 + a)/2, 1/(a +7), paste0("Area: ", area))
}
