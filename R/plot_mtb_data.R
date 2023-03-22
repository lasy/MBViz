

#' Plot the explanatory and response variables
#'
#' @param Xs a `list` with the explanatory blocks
#' @param Y a single element `list` with the response block
#'
#' @return a `patchwork` of `ggplot2` object
#' @export
#'
#' @import ggplot2
#' @import patchwork
#' @importFrom stats weighted.mean
plot_mtb_data <- function(Xs, Y){

  j <-
    apply(Y[[1]], 1, function(x) weighted.mean(seq_along(x), x)) %>%
    order(., decreasing = TRUE)
  id_sort <- rownames(Y[[1]])[j]

  Xs_long <- get_mtb_blocks(Xs)
  Y_long <- get_mtb_blocks(Y)

  max <- max(abs(c(Xs_long$value, Y_long$value))) %>% ceiling()

  g_inputs <-
    plot_mtb_block_data(df = Xs_long, max = max, id_sort = id_sort) +
    ggtitle("Explanatory\nvariables") +
    theme(
      panel.border = element_blank(),
      strip.text = element_text(color = "black")
    )
  g_output <-
    plot_mtb_block_data(df = Y_long, max = max, id_sort = id_sort) +
    ggtitle("Response\nvariables") +
    theme(
      panel.border = element_blank(),
      strip.text = element_text(color = "black")
    )

  g_inputs + g_output +
    plot_layout(
      widths = c(lengths(Xs) %>% sum, lengths(Y) %>% sum),
      guides = "collect"
    )
}


#' Extract block data into long format
#'
#' @param blocks
#'
#' @return a `tibble`
#' @keywords internal
#' @importFrom purrr map_dfr
#' @importFrom stringr str_replace
get_mtb_blocks <- function(blocks) {

  purrr::map_dfr(
    .x = seq_along(blocks),
    .f = function(i, blocks) {
      blocks[[i]] %>%
        scale() %>%
        as_tibble() %>%
        mutate(id = rownames(blocks[[i]]), block = names(blocks)[i]) %>%
        pivot_longer(cols = -c(id, block))
    },
    blocks = blocks
  ) %>%
    mutate(
      block =
        block %>% str_replace(" ","\n") %>%
        factor(., levels = names(blocks) %>% str_replace(" ","\n")),
      name =
        name %>%
        factor(., levels = blocks_and_variables(blocks)$variable %>% levels())
    )
}

plot_mtb_block_data <- function(df, max, id_sort) {

  ggplot(df %>% mutate(id = id %>% factor(., levels = id_sort)),
         aes(x = name, y = id, fill = value)) +
    geom_tile() +
    xlab("") +
    scale_y_discrete("", breaks = NULL) +
    scale_fill_gradientn(
      # colors = c("coral4","coral", "gray95", "steelblue1", "steelblue4"),
      colors = c("goldenrod3","goldenrod1", "gray95", "steelblue1", "steelblue4"),
      values = c(0, 0.4, 0.5, 0.6, 1), limits = c(-max, max)
    ) +
    facet_grid(. ~ block, scales = "free_x", space = "free_x") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
