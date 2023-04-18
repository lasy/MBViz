
#' Scatter plot of two variables against each other
#'
#' @param Xs (optional) the multiblock input (list of `matrices`/`data.frames`)
#' @param Y (optional) the output `matrix`/`data.frame`
#' @param res (optional - either `res` or `Xs` and `Y`) the result of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param varX a character specifying the x-axis variable (can be an explanatory or response variable)
#' @param varY a character specifying the y-axis variable (can be an explanatory or response variable)
#' @param color_by (optional) a character or a vector of the same length of the data
#' specifying how the samples should be colored
#'
#' @return a `ggplot2` object
#' @export
#'
plot_varX_varY <- function(Xs = NULL, Y = NULL, res = NULL, varX, varY, color_by = NULL) {

  if (!is.null(res)) {
    X <- res$tabX
    Y <- res$tabY
  } else {
    if (is.null(Xs) | is.null(Y)) stop("Xs and Y or res must be provided.\n")
    X <- blocks_to_df(Xs)
  }

  if (!is.null(color_by)) {
    if (is.character(color_by) & (length(color_by) == 1)){
      if (color_by %in% colnames(X)) colors <- Xs[,color_by]
      else if (color_by %in% colnames(Y)) colors <- Y[, color_by]
      else stop("If `color_by` is a character, it should be the name of a Xs or a Y variable.")
    } else if (length(color_by) == nrow(X)) {
      colors <- color_by
    } else
      stop("color_by must be a character providing the name of an X variable or
                 a value vector with the same length as the dimension of the data.\n")
  } else {
    colors <- "A"
  }


  df <- tibble(x = X[,varX], y = Y[,varY], color = colors)

 g <-
   ggplot(df, aes(x = x, y = y, col = color)) +
    geom_point(alpha = 0.5, size = 0.75) +
    geom_smooth(method = "lm", formula = "y ~ x", aes(fill = color)) +
    xlab(varX) + ylab(varY)

  if (all(colors == "A")) {
    g <-
      g + guides(col = "none", fill = "none") +
      scale_color_manual(values = "black") +
      scale_fill_manual(values = "gray50")
  }

 g
}

add_Y_vars_to_blocks_and_variables <- function(input_vars, Y) {
  all_vars <-
    input_vars %>%
    dplyr::mutate(
      block = block %>% forcats::fct_expand("Y"),
      pretty_block = pretty_block %>% forcats::fct_expand("Y"),
      variable = variable %>% forcats::fct_expand(colnames(Y))
    )
  all_vars <-
    all_vars %>%
    dplyr::bind_rows(
      dplyr::tibble(
        block = "Y" %>% factor(., levels = levels(all_vars$block)),
        variable = colnames(Y) %>% factor(., levels = levels(all_vars$variable)),
        pretty_block = "Y" %>% factor(., levels = levels(all_vars$pretty_block))
        )
    )
}
