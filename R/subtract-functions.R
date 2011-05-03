subtractExpectation <- function(fg, bg, indTss, basal, exppara) {

  n <- length(fg)
  
  fak <- 1/.exppdf(1, exppara)

  win1 <- fak[1]*.exppdf(n:1, exppara[1])
  win2 <- fak[2]*.exppdf(1:n, exppara[2])
  win <- c(win1, 0, win2)

  bgb <- rep(0, n)
  bgb[indTss] <- bg[indTss]
  cums <- convolve(win, rev(bgb), type="open")
  
  expect <- cums[(n+1L):(length(cums)-n)]
  expect[expect < basal] <- basal

  delta <- fg - expect
  delta[delta < 0] <- 0

  res <- list(delta=delta, expect=expect)
}
