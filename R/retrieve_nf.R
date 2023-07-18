#' Retrieve the number of latent factors used for the bootstrap CI estimations
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`) (the input of `ade4::randboot`)
#' @param boot (optional) the output of `ade4::randboot` to display the confidence intervals
#'
#' @return an `integer`
#' @keywords internal
retrieve_nf <- function(res, boot){
  if ("mbplsda" %in% class(res)) {
    nf <- ifelse(is.null(boot), res$nf, length(boot$faX))
  } else {
    nf <- ifelse(is.null(boot), res$nf, which.min(abs(boot$bipc$obs[1] - res$bipc[1,])))
  }
  nf
}
