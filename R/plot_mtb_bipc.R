
#' Plots the observed (and bootstrapped) cumulative Block Importance in Projections (BIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param show_dist (optional) a `logical` specifying if the bootstrap distribution should be shown with a boxplot.
#' Default is FALSE
#' @param CI The confidence interval that should be displayed. Default is 0.95.
#' @param wrap_block_names (optional) `integer` specifying if the block names should be wrapped to the specified character length. Default is `NULL`, indicating no wrapping.
#' @param show_ref (optional) a `logical` specifying if the reference value under the assumption of equal variable importance should be shown. Default is `TRUE`.
#'
#' @return a `ggplot2` object
#' @export
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr as_tibble mutate select
#' @importFrom stats quantile
#' @importFrom forcats fct_inorder
#' @importFrom stringr str_wrap
#' @import ggplot2
plot_mtb_bipc <- function(res, boot = NULL, show_dist = FALSE, CI = 0.95, show_ref = TRUE, show_ref_ratio = FALSE, wrap_block_names = NULL) {

  bipc <- get_mtb_bipc(res = res, boot = boot, CI = CI)

  if (is.null(boot)) bipc <- bipc %>% mutate(lo = 0, up = value)
  if (show_ref) bipc <- bipc |> left_join(get_ref_bipc(res = res), by = join_by(block))
  if (!is.null(wrap_block_names))
    bipc <- bipc |> arrange(block) |>  mutate(block = block |> stringr::str_wrap(wrap_block_names) |> forcats::fct_inorder())

  ylab <- "Cummulative\nBlock Importance"

  if (show_ref_ratio) {
    bipc <- bipc |> mutate(value = value / bipc_ref, lo = lo / bipc_ref, up = up / bipc_ref)
    ylab <- str_c("Relative\n", ylab)
  }

  g_bipc <-
    ggplot(bipc, aes(x = block)) +
    geom_hline(yintercept = 0) +
    # geom_hline(yintercept = 1/nrow(bipc), linetype = 3, col = "gray70") +
    expand_limits(y = 0) +
    guides(fill = "none", col = "none") +
    xlab("") +
    ylab(ylab) +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  if (show_ref_ratio) {
    g_bipc <- g_bipc +
      geom_hline(yintercept = 1, linetype = 3, col = "gray70")
  }

  if (show_dist) {
    g_bipc <-
      g_bipc +
      geom_boxplot(
        data = bootstraps_bipc,
        aes(col = block, fill = block),
        alpha = 0.5
      ) +
      geom_point()
  } else {
    g_bipc <-
      g_bipc +
      geom_segment(
        aes(xend = block, y = lo, yend = up, col = block),
        alpha = ifelse(is.null(boot), 1, 0.5),
        linewidth = ifelse(is.null(boot), 0.5, 2),
        lineend = "round"
      )
    if (show_ref & ! show_ref_ratio) {
      g_bipc <-
        g_bipc +
        geom_segment(
          data = bipc |> mutate(x = block |> as.numeric()),
          aes(x = x - 0.3, xend = x + 0.3, y = bipc_ref, yend = bipc_ref),
          col = "gray50", linetype = 3, lineend = "round"
        )
    }
    g_bipc <- g_bipc + geom_point(aes(y = value, col = block), size = 3)
  }

  g_bipc

}
