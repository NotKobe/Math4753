#' My Function producing z values
#'
#' Produce vectors of z values
#'
#'
#'
#' @param x a vector
#'
#' @return a list of vectors
#' @export
#'
#' @examples x = 1:7; myZfunction(x)
myZfunction = function(x){
  z = (x - mean(x))/sd(x)
  z
}
