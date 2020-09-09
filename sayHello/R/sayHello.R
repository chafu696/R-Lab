#' This is some description
#' @title sayHello 
#' 
#' @description the function paste two words.
#' @details you can use this function.
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#' 
#' @param name name is a word
#' 
#' @return a word
#' 
#' @export
#' @examples name="FUCHAO";sayHello(name)

sayHello <- function(name){
  return(paste("Hello",name))
}
