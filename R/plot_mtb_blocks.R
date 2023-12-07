
#' Displays the blocks structure
#'
#' @param Xs a `list` with the explanatory blocks
#' @param Y a single element `list` with the response block
#' @param response_first (optional) a `logical` specifying if the response block
#' should be on the left (`response_first = TRUE`) or on the right
#' (`response_first = FALSE`, default) of the explanatory variables.
#' @param min_text_size (optional) a positive `double` specifying the minimum
#' for the block variable text size.
#' @param max_text_size (optional) a positive `double` specifying the maximum
#' for the block variable text size.
#' @param title_sep (optional) a `character` specifying the separating character
#' for the title (default is `"\n"`).
#' @param add_n (optional) a `logical` specifying if the number of samples should
#' be specified on the y-axis (default is `TRUE`)
#' @param add_p (optional) a `logical` specifying if the number of variables should
#' be specified on the x-axis (default is `TRUE`)
#' @param block_colors (optional) a `character` vector specifying the colors of
#' the explanatory blocks. If `NULL` (default), the colors are the default ggplot colors.
#'
#' @return a `ggplot2` object
#' @export
#' @import patchwork
#' @importFrom ggplot2 ggtitle
#' @importFrom stringr str_c str_length
plot_mtb_blocks <- function(Xs, Y, response_first = FALSE,
                            min_text_size = 3, max_text_size = 4,
                            title_sep = "\n", add_n = TRUE, add_p = TRUE,
                            block_colors = NULL){
  Xs_vars <- .blocks_and_variables_from_list(Xs)
  Y_vars <- .blocks_and_variables_from_list(Y)
  text_size_range <- range(str_length(c(Xs_vars$variable, Y_vars$variable)))

  g_Xs <-
    .plot_mtb_blocks(
      Xs_vars,
      min_text_size = min_text_size, max_text_size = max_text_size,
      text_size_range = text_size_range,
      add_p = add_p
      )

  if (!is.null(block_colors)) {
    g_Xs <-
      g_Xs +
      scale_color_manual(values = block_colors) +
      scale_fill_manual(values = block_colors)
  }

  g_Y <-
    .plot_mtb_blocks(
      Y_vars,
      min_text_size = min_text_size, max_text_size = max_text_size,
      text_size_range = text_size_range
    ) +
    scale_color_manual(values = "gray40") +
    scale_fill_manual(values = "gray40")

  if (add_n & response_first) g_Y <- g_Y + ylab(str_c("n = ", nrow(Xs[[1]])))
  if (add_n & !response_first) g_Xs <- g_Xs + ylab(str_c("n = ", nrow(Xs[[1]])))

  Xs_title <- str_c("Explanatory", title_sep,"variables")
  Y_title <- str_c("Response", title_sep, "variables")
  if (add_n & add_p) {
    Xs_title <-
      Xs_title |> str_c(title_sep,"(", nrow(Xs[[1]]) ," x ", nrow(Xs_vars),")")
    Y_title <-
      Y_title |> str_c(title_sep, "(", nrow(Xs[[1]]) ," x ", nrow(Y_vars),")")
  }
  g_Xs <- g_Xs + ggtitle(Xs_title)
  g_Y <- g_Y + ggtitle(Y_title)

  if (response_first){
    patch <- g_Y + g_Xs + plot_layout(widths = c(nrow(Y_vars), nrow(Xs_vars)))
  } else {
    patch <- g_Xs + g_Y + plot_layout(widths = c(nrow(Xs_vars), nrow(Y_vars)))
  }
  patch
}

#' Plots the blocks from a list
#'
#' @param input_var the output of `blocks_and_variable()`
#' @param min_text_size (optional) a positive `double` specifying the minimum
#' for the block variable text size.
#' @param max_text_size (optional) a positive `double` specifying the maximum
#' for the block variable text size.
#' @param text_size_range a `vector` of two integers specifying the lengths of
#' the shortest and longuest variable name
#' @param add_p (optional) a `logical` specifying if the number of variables should
#' be specified on the x-axis (default is `TRUE`)
#' @return a `ggplot2` object
#' @keywords internal
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom stringr str_length
.plot_mtb_blocks <- function(input_var, min_text_size = 3, max_text_size = 4, text_size_range, add_p = FALSE) {
  lims <- rev(-(text_size_range + 0.1 * c(-1, 1)))
  input_var <- input_var %>% mutate(var_length = str_length(variable))
  block_names <- input_var$block %>% unique()
  ggplot(
    input_var,
    aes(x = variable, y = 1)) +
    geom_tile(aes(fill = block), col = "white", alpha = 0.2) +
    geom_text(aes(label = variable, col = block, size = -var_length), angle = 90) + #
    facet_grid(. ~ pretty_block, scales = "free", space = "free") +
    scale_x_discrete(ifelse(add_p, str_c(nrow(input_var)," var."),""), breaks = NULL) +
    scale_y_continuous(breaks = NULL) + ylab("") +
    scale_size(range = c(min_text_size, max_text_size), limits = lims) + #
    guides(col = "none", fill = "none", size = "none") +
    theme(
      strip.text = element_text(color = "black")
    )

}

