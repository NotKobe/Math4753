#' myci
#'
#' @param x a vector, your data
#' @param mean an integer, the mean of your
#' @param n an integer, the number of samples
#' @param ...
#'
#' @return a confidence interval with lower and upper tail to your accuraracy desire
#' @export
#'
#' @examples myci(x = data, alpha = 0.05, n = 25)
myci = function(x, alpha = 0.05, n = 30,...){

  plus_minus = c(-1,1)
  mean(x) + plus_minus*qt(1-.05/2,n-1)*sd(x)/sqrt(n) # 95%

  }
