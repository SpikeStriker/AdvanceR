euclidean <- function(a, b) {
  stopifnot(is.numeric(a) & is.numeric(b) & length(a) == 1 & length(b) == 1)
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  
  return(a)
}