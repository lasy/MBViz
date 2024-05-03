
#' Compute reference cummulative block importance importance
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv` or `packMBPLSDA::mbplsda`)
#' @return a `tibble` object
#' @export
#' @importFrom magrittr set_colnames extract
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr tibble as_tibble mutate select left_join group_by summarize rename
get_ref_bipc <- function(res){
  tibble(
    variable = colnames(res$tabX),
    v = res$tabX |> apply(2, var)
  ) |>
    left_join(res$TC |> set_colnames(c("block", "variable")), by = join_by(variable)) |>
    group_by(block) |>
    summarize(a2 = sum(v)) |>
    mutate(bipc_ref = a2/sum(a2))
}

#' Compute reference cummulative variable importance importance
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv` or `packMBPLSDA::mbplsda`)
#' @return a `tibble` object
#' @export
#' @importFrom magrittr set_colnames extract
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr tibble as_tibble mutate select left_join group_by summarize rename
get_ref_vipc <- function(res){
  tibble(
    variable = colnames(res$tabX),
    v = res$tabX |> apply(2, var)
  ) |>
    left_join(res$TC |> set_colnames(c("block", "variable")), by = join_by(variable)) |>
    left_join(get_ref_bipc(res) |> select(block, bipc_ref) |> dplyr::rename(bipc_ref_null = bipc_ref), by = join_by(block)) |>
    left_join(
      tibble(
        block = unique(res$TC$T),
        bipc =  res$bipc |> extract(,ncol(res$bipc))
      ), by = join_by(block)
    ) |>
    mutate(vipc_ref = v * bipc/sum(v * bipc))
    # mutate(vipc_ref = v * bipc^2/sum(v * bipc^2))
}
