#' @title euclidean
#' 
#' @description Gives the highest common divisor of two integers. 
#' @details Uses the Euclidean algorithm to find the greatest common divisor between two numbers.
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' 
#' @param x A numeric scalar
#' @param y A numeric scalar
#' 
#' @return The highest common divisor of \code{x} and \code{y}
#'
#' @examples euclidean(700, 20)
#' @examples euclidean(-5552, 1515126)
#' 
#' @export

euclidean <- function (x, y) {
  stopifnot(is.numeric(x), is.numeric(y), length(x)==1, length(y)==1)
  while (y != 0) {
    t <- y
    y <- x%%y
    x <- t
  }
  return(x)
}

