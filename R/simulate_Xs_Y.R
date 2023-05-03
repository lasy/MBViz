
simulate_Xs_Y <- function(
    n = 200, p = 2:5, q = 4, nlX = 2, nlY = 3, snr = 1,
    Bxx_function = sample_B_random, Bxy_function = sample_B_random, By_function = sample_B_random,
    categorical_Y = FALSE
    ){
  P <- sum(p)

  e <- matrix(rnorm(n * P), nrow = n, ncol = P) %>% scale() # noise matrix for X
  f <- matrix(rnorm(n * q), nrow = n, ncol = q) %>% scale() %>% set_colnames(paste0("Y",1:q)) # noise matrix for Y

  lX <- matrix(rnorm(n * nlX), nrow = n, ncol = nlX) %>% scale()
  lY <- matrix(rnorm(n * nlY), nrow = n, ncol = nlY) %>% scale()

  Bxx <- Bxx_function(nlX, P)
  Bxy <- Bxy_function(nlY, P)
  By  <- By_function(nlY, q)
  A <-
    compute_Xs_Y(
      Bxx = Bxx, Bxy = Bxy, By = By,
      lX = lX, lY = lY, p = p,
      e = e, f = f, snr = snr,
      categorical_Y = categorical_Y
    )

  A


}

compute_Xs_Y <- function(Bxx = 0, Bxy = 0, By = 0, lX = 0, lY = 0, p, e, f, snr = 1, categorical_Y = FALSE){
  if (length(Bxx) == 1) LXX <- 0*e else LXX <- lX %*% Bxx
  if (length(Bxy) == 1) LXY <- 0*e else LXY <- lY %*% Bxy

  SX <- LXX + LXY
  SX <- SX %>% scale_non_zero()
  Xs_mat <- (e/snr + SX)
  Xs_blocks <- Xs_mat %>% as.data.frame() %>% as_blocks(., p = p)
  Xs_mat <- Xs_blocks %>% blocks_to_df() %>% as.matrix()

  if (length(By) == 1) LY <- 0*f else LY <- lY %*% By
  Y <- f/snr + scale_non_zero(LY)
  if (categorical_Y)
    Y <- data.frame(Y = apply(Y, 1, which.max) %>% factor()) %>% packMBPLSDA::disjunctive()

  list(
    Xs = Xs_blocks, Xs_mat = Xs_mat, Y = Y,
    Bxx = Bxx, Bxy = Bxy, By = By,
    lX = lX, lY = lY, p = p,
    e = e, f = f, snr = snr
  )
}


scale_non_zero <- function(A) {
  i <- (apply(A, 2, sd) != 0)
  newA <- A
  if (sum(i) > 0) newA[, which(i)] <- scale(A[, which(i)], center = TRUE, scale = TRUE)
  newA
}
