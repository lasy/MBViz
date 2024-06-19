#' Add colors to a data frame
#'
#' @param X a `data.frame`
#' @param sample_color a `vector` or a `data.frame` with the colors of the samples
#'
#' @return a `data.frame` object
#' @export
#'
#' @importFrom dplyr mutate left_join join_by
#' @importFrom tidyr pivot_longer
.add_colors <- function(X, samples_color) {
  if (!is.null(samples_color)) {
    if (
      (!is.null(length(samples_color)) && (length(samples_color) == nrow(X))) |
      (is.array(samples_color) &&
       ((ncol(samples_color) == 1) & (nrow(samples_color) == nrow(X))))
    ) {
      X <- X %>% mutate(variable = samples_color %>% unlist(), value = 1)
    } else if (
      (nrow(samples_color) == nrow(X)) &
      (all(round(rowSums(samples_color),5) == 1))
      ){
      samples_color_long <-
        samples_color %>%
        mutate(sampleID = rownames(samples_color)) %>%
        pivot_longer(cols = -sampleID, names_to = "variable", values_to = "value")
      X <-
        X %>%
        left_join(., samples_color_long, by = join_by(sampleID), multiple = "all")
    } else {
      stop(
        "samples color must be a vector (same length as number of samples) or
        a matrix/data.frame (same number of rows as the data) whose rows sum to 1\n")
    }
  }

  X
}
