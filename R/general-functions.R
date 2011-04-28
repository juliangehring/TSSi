## exppdf ##
.exppdf <- function(x, mu) {

  res <- 1/mu * exp(-x/mu)

  return(res)
}
