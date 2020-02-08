#' My Function of sqaures and cubes
#'
#' Produce list of squares and cubes
#'
#' This isa part of the introduction to package package making
#'
#' @param x a vector
#'
#' @return a list of vectors
#' @export
#'
#' @examples x = 1:4; myf(x)
myf = function(x){
  obj1 = x ^ 2
  obj2 = x ^ 3
  list(square = obj1, cube = obj2)
}
