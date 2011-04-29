subtractExpectation <- function(fg, bg, indTss, basal, exppara) {

  n <- length(fg)
  
  fak <- 1/.exppdf(1, exppara)

  win1 <- fak[1]*.exppdf(1:n, exppara[1])
  win2 <- fak[2]*.exppdf(1:n, exppara[2])
  win <- c(rev(win1), 0, win2)

  cum <- convolve(bg[indTss], win, type="open")
  
  expect <- cum[(n+1):(length(cum)-n)]
  expect[expect < basal] <- basal

  delta <- fg - expect
  delta[delta < 0] <- 0

  res <- list(delta=delta, expect=expect) ## fg not changed, thus not exported
}
