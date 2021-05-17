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