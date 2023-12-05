
#' Predict Y from an `ade4::mbpls` model
#'
#' @param res the output of an `ade4::mbpls` or `packMBPLSDA::mbplsda` call
#' @param newdata (optional) an `ade4::ktab` object - similar to what would be
#' provided as input of `ade4::mbpls`.
#' If `newdata` is not provided, the predictions for the original data are provided.
#' @param original_X (mandatory if newdata is not null) the original `ade4::ktab`
#' object on whicht the model (`res`) was fitted
#' @param original_Y (optional) the `matrix` or `data.frame` that was provided to the `res` model.
#' If not provided, the predictions will be centered and scaled.
#' @return a `matrix` with the predicted output.
#' @export
mtb_predict <- function(res, original_Y = NULL, newdata = NULL, original_X = NULL) {

  if (is.null(newdata)) {
    newX <- res$tabX
  } else {
    if (is.null(original_X)) stop("If new data is provided, then the original ktabX must be provided too\n")
    if (sum(newdata$blo) != sum(original_X$blo)) stop("`newdata` must have the same number of variable as the original data")
    newX <- newdata %>% ktab2matrix()
    oldX <- original_X %>% ktab2matrix()
    Xvars <- colnames(oldX)
    for (Xvar in Xvars) {
      newX[,Xvar] <-
        tibble(actual_X = oldX[,Xvar], mbpls_X = res$tabX[,Xvar]) %>%
        lm(mbpls_X ~ actual_X, data = .) %>%
        predict(., newdata = tibble(actual_X = newX[,Xvar]))
    }
  }


  newX <- newX %>% as.matrix()

  coefs <- get_mtb_coef(res = res, as_matrix = TRUE)

  Y_hat <- newX %*% coefs

  # linear transformation to go from the predicted Y to the original Y
  if (!is.null(original_Y)) {
    for (i in 1:ncol(Y_hat)) {
      Y_hat[,i] <-
        tibble(actual_Y = original_Y[,i], mbpls_Y = res$tabY[,i]) %>%
        lm(actual_Y ~ mbpls_Y, data = .) %>%
        predict(., newdata = tibble(mbpls_Y = Y_hat[,i]))
    }
  }

  Y_hat
}



ktab2matrix <- function(ktab){
  X <- unclass(ktab)[1:length(ktab$blo)] %>% purrr::list_cbind()
  X <- X %>% as.matrix()
  colnames(X) <- col.names(ktab)
  X
}
