
#' Plots the observed (and bootstrapped) cumulative Block Importance in Projections (BIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param show_dist (optional) a `logical` specifying if the bootstrap distribution should be shown with a boxplot.
#' Default is FALSE
#' @param CI The confidence interval that should be displayed. Default is 0.95.
#' @param wrap_block_names (optional) `logical` specifying if block names (x-axis) should be wrapped. Default is `TRUE`.
#'
#' @return a `ggplot2` object
#' @export
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom dplyr as_tibble mutate select
#' @importFrom stats quantile
#' @import ggplot2
plot_mtb_bipc <- function(res, boot = NULL, show_dist = FALSE, CI = 0.95, wrap_block_names = TRUE) {

  bipc <- get_mtb_bipc(res = res, boot = boot, CI = CI)

  if (is.null(boot)) bipc <- bipc %>% mutate(lo = 0, up = value)
  if (wrap_block_names)
    bipc <- bipc %>% mutate(x = pretty_block)
  else
    bipc <- bipc %>% mutate(x = block)

  g_bipc <-
    ggplot(bipc ,
           aes(x = x, y = value)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1/nrow(bipc),
               linetype = 3, col = "gray70") +
    expand_limits(y = 0) +
    guides(fill = "none", col = "none") +
    xlab("") +
    ylab("Cummulative\nBlock Importance") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
        aes(xend = x, y = lo, yend = up, col = block),
        alpha = ifelse(is.null(boot), 1, 0.5),
        linewidth = ifelse(is.null(boot), 0.5, 2),
        lineend = "round"
      ) +
      geom_point(aes(col = block), size = 3)
  }

  g_bipc

}
