
#' Simulate "multiblock" data
#'
#' @param n (an `integer`) the number of samples.
#' @param p (a `vector` of `integer`s) the number of variable in each block.
#' The number of block is specified by the length of `p`.
#' @param q (an `integer`) the number of response variables.
#' @param nlX (an `integer`) the number of latent variables for the
#' explanatory variables only.
#' @param nlY (an `integer`) the number of latent variables shared by the
#' explanatory and response variables.
#' @param snr (a strictly positive `double`) the signal to noise ratio.
#' @param Bxx_function (a `function`) a function specifying how the explanatory
#' variables depend on the X-only latent factors. `sample_B_random` is the default.
#' @param Bxy_function (a `function`) a function specifying how the explanatory
#' variables depend on the shared latent factors. `sample_B_random` is the default.
#' @param By_function (a `function`) a function specifying how the response
#' variables depend on the shared latent factors. `sample_B_random` is the default.
#' @param categorical_Y (a `logical`) whether the response variables are Bernoulli.
#'
#' @return a `list` with the simulated data.
#' @export
#'
#' @import magrittr
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

#' Computes the simulated data
#'
#' @param Bxx (optional) a `nlX x sum(p)` `matrix` specifying the relationship
#' between the explanatory variables and the X-only latent factors.
#' Default = no relationship.
#' @param Bxy (optional) a `nlY x sum(p)` `matrix` specifying the relationship
#' between the explanatory variables and the shared latent factors.
#' Default = no relationship.
#' @param By (optional) a `nlY x sum(p)` `matrix` specifying the relationship
#' between the explanatory variables and the shared latent factors.
#' Default = no relationship.
#' @param lX a `n x nlX` `matrix` with the X-only latent variables.
#' @param lY a `n x nlY` `matrix` with the shared latent variables.
#' @param p a `vector` of `integer`s with the number of variables in each block.
#' @param e a `n x sum(p)` `matrix` with the explanatory variables noise.
#' @param f a `n x q` `matrix` with the response variables noise.
#' @param snr a strictly positive `double` specifying the signal-to-noise ratio.
#' @param categorical_Y
#'
#' @return a `list` with the computed data
#' @keywords internal
#' @import magrittr
compute_Xs_Y <- function(Bxx = 0, Bxy = 0, By = 0, lX, lY, p, e, f, snr = 1, categorical_Y = FALSE){
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
