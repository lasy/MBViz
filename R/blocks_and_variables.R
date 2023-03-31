#' Creates a tibble with the block and variable names
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#'
#' @return a `tibble`
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
blocks_and_variables <- function(res) {
  tibble(
    block = rep(names(res$blo), res$blo) %>% factor(., levels = names(res$blo)),
    variable = colnames(res$tabX) %>% factor(., levels = colnames(res$tabX))
  )
}


#' Creates a tibble with the block and variable names
#'
#' @param inputs a list of matrices or data.frames
#'
#' @return a `tibble`
#' @keywords internal
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
.blocks_and_variables_from_list <- function(inputs) {
  map_dfr(
    .x = names(inputs),
    .f = function(input, inputs){
      variables <- colnames(inputs[[input]])
      if (is.null(variables)) variables <- str_c(input, "_", 1:ncol(inputs[[input]]))
      tibble(block = input, variable = variables)
    },
    inputs = inputs
  ) %>%
    mutate(
      block = block %>% factor(., levels = unique(block)),
      variable = variable %>% factor(., levels = unique(variable))
    )
}

