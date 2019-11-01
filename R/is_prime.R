is_prime <- function(x) {
  if (!is.numeric(x)) {
    stop("'x' should be numeric")
  }
  if (x == 2) {
    return(TRUE)
  } else {
    for (i in 2:(x - 1)) {
      if (x %% i == 0) {
        return(FALSE)
      }
    }
  }
}
