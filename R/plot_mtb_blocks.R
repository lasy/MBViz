
#' Displays the blocks structure
#'
#' @param Xs a `list` with the explanatory blocks
#' @param Y a single element `list` with the response block
#'
#' @return a `ggplot2` object
#' @export
#' @import patchwork
#' @importFrom ggplot2 ggtitle
#' @importFrom stringr str_c
plot_mtb_blocks <- function(Xs, Y){
  Xs_vars <- .blocks_and_variables_from_list(Xs)
  Y_vars <- .blocks_and_variables_from_list(Y)
  g_Xs <-
    .plot_mtb_blocks(Xs_vars) +
    ggtitle("Explanatory\nvariables") +
    ylab(str_c("n = ", nrow(Xs[[1]])))
  g_Y <-
    .plot_mtb_blocks(Y_vars) +
    ggtitle("Response\nvariables") +
    scale_color_manual(values = "gray40") +
    scale_fill_manual(values = "gray40")
  g_Xs + g_Y + plot_layout(widths = c(nrow(Xs_vars), nrow(Y_vars)))
}

#' Plots the blocks from a list
#'
#' @param input_var the output of `blocks_and_variable()`
#'
#' @return a `ggplot2` object
#' @keywords internal
#' @import ggplot2
#' @importFrom dplyr mutate
.plot_mtb_blocks <- function(input_var) {
  block_names <- input_var$block %>% unique()
  ggplot(
    input_var %>%
      mutate(
        pretty_block = str_wrap(block, width = 12),
        pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
      ),
    aes(x = variable, y = 1)) +
    geom_tile(aes(fill = block), col = "white", alpha = 0.2) +
    geom_text(aes(label = variable, col = block), angle = 90) +
    facet_grid(. ~ pretty_block, scales = "free", space = "free") +
    scale_x_discrete("", breaks = NULL) +
    scale_y_continuous(breaks = NULL) + ylab("") +
    guides(col = "none", fill = "none") +
    theme(
      strip.text = element_text(color = "black")
    )

}

