#' Creates a tibble with the block and variable names
#'
#' @param inputs a list of matrices or data.frames
#'
#' @return a `tibble`
#' @export
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr %>%
get_block_data <- function(inputs) {
  map_dfr(
    .x = names(inputs),
    .f = function(input, inputs){
      tibble(block = input, variable = colnames(inputs[[input]]))
    },
    inputs = inputs
  ) %>%
    mutate(
      block = block %>% factor(., levels = unique(block)),
      variable = variable %>% factor(., levels = unique(variable))
    )
}
