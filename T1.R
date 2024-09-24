euclidean <- function(a, b) {
  stopifnot(is.numeric(a) & is.numeric(b) & length(a) == 1 & length(b) == 1) # Check for single numeric values
  while (b != 0) {
    temp <- b # Store value b in a temporary variable
    b <- a %% b # Update value of b to remainder of a divided by b
    a <- temp # Assign old value of b to a
  }
  
  # GCD equal to last value of a once b equal to zero
  return(a)
}
