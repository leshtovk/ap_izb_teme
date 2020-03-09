fact <- function(n) {
  if (n == 2) {
    return(2)
  }
  else {
    return(n * fact(n-1))
  }
}