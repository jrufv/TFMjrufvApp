#' Combinatoria
#' 
#' @param x Vector de caracteres.
#' @return Vector con la combinación de elementos de \code{x}.
#' @export
#' @examples 
#' vector <- c("A", "B", "C")
#' combina(vector)
 
combina <- function(x) {
  vec <- c()
  for(i in 1:(length(x)-1)) {
    for(j in 1:(length(x)-i)) {
      vec_ij <- paste0(x[i], " - ", x[i+j])
      vec <- c(vec, vec_ij)
    }
  }
  return(vec)
}

#' Combinatoria 2
#'
#' @param x Vector de caracteres.
#' @return Vector con la combinación de elementos de \code{x}. Cada elemento de
#'   \code{x} estará introducido en un paréntesis.
#' @export
#' @examples
#' vector <- c("A-B", "A-C", "B-C")
#' combina_int(vector)

combina_int <- function(x) {
  vec <- c()
  for(i in 1:(length(x)-1)) {
    for(j in 1:(length(x)-i)) {
      vec_ij <- paste0("(", x[i], ") - (", x[i+j], ")")
      vec <- c(vec, vec_ij)
    }
  }
  return(vec)
}