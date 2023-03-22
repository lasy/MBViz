#' Extract Eigenvalues from a `ade4::mbpls` or `ade4::mbpcaiv` result
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#'
#' @return a `tibble`
#' @export
#'
#' @importFrom dplyr tibble mutate
get_mtb_eig <- function(res) {
  tibble(Ax = 1:length(res$eig), eig = res$eig) %>%
    mutate(
      var = eig/sum(eig),
      cumvar = cumsum(var)
    )
}
