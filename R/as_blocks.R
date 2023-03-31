#' Transform a matrix into a list of blocks
#'
#' @param M the `matrix` to be transformed in blocks
#' @param p an `integer` `vector` with the number of variable in each block
#'
#' @return a named `list` of matrices
#' @export
#' @import magrittr
#' @importFrom purrr map
as_blocks <- function(M, p){
  name_columns <- is.null(colnames(M))
  if (is.null(names(p))) block_names <- LETTERS[1:length(p)] else block_names <- names(p)
  M %>%
    purrr::map(
      .x = 1:length(p),
      .f = function(M, i) {
        M_i <- M[,(1:p[i]) + c(0,cumsum(p))[i]]
        if (name_columns) M_i <- M_i %>% set_colnames(str_c(block_names[i],"_", 1:p[i]))
      },
      M = .)  %>%
    set_names(block_names)
}


#' Convert block lists to a `data.frame`
#'
#' @param L a `list` of matrices or data.frames
#'
#' @return a `tibble` (`data.frame`)
#' @export
#'
#' @importFrom purrr map_dfc
blocks_to_df <- function(L) {
  purrr::map_dfc(.x = L, .f = as.data.frame)
}
