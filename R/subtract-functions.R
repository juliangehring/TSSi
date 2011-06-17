## subtractExpectation ##
subtractExpectation <- function(fg, bg, indTss, pos, basal, tau) {

  idx <- pos - pos[1L] + 1L
  idxTss <- idx[indTss]
  n <- pos[length(fg)] - pos[1L] + 1L

  fak <- 1/.exppdf(1, tau)

  win1 <- fak[1]*.exppdf(n:1, tau[1])
  win2 <- fak[2]*.exppdf(1:n, tau[2])
  win <- c(win1, 0, win2)

  bgb <- rep(0, n)
  bgb[idxTss] <- bg[indTss]
  cums <- convolve(win, rev(bgb), type="open")
  
  expect <- cums[(n+1L):(length(cums)-n)][idx]
  expect[expect < basal] <- basal

  delta <- fg - expect
  delta[delta < 0] <- 0

  res <- list(delta=delta, expect=expect)

  return(res)
}
