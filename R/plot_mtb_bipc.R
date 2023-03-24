
#' Plots the bootstraped and observed Block Importance
#'
#' @param boot the output of `ade4::randboot`
#' @param input_var a `data.frame` xxxx
#' @param show_dist (optional) a `logical` specifying if the bootstrap distribution should be shown with a boxplot.
#' Default is FALSE
#' @param CI The confidence interval that should be displayed. Default is 0.95.
#'
#' @return a `ggplot2` object
#' @export
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate
#' @importFrom stats quantile
#' @import ggplot2
plot_mtb_bipc <- function(boot, input_var, show_dist = FALSE, CI = 0.95) {

  bootstraps_bipc <-
    boot$bipc$boot %>%
    t() %>%
    set_colnames(1:nrow(boot$bipc$boot)) %>%
    as_tibble() %>%
    mutate(
      block =
        colnames(boot$bipc$boot) %>%
        factor(., levels = input_var$block %>% levels())
    ) %>%
    pivot_longer(
      cols = -block,
      names_to = "b",
      values_to = "value"
    )

  obs_bipc <-
    tibble(
      block = names(boot$bipc$obs) %>%
        factor(., levels = input_var$block %>% levels()) ,
      value = boot$bipc$obs
    )

  blocks <- obs_bipc$block


  g_bipc <-
    ggplot(obs_bipc ,
           aes(x = block, y = value)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1/length(unique(bootstraps_bipc$block)),
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
    bootstraps_bipc_summary <-
      bootstraps_bipc %>%
      group_by(block) %>%
      summarize(
        lo = quantile(value, p = (1-CI)/2),
        up = quantile(value, p = CI + (1-CI)/2)
        )

    g_bipc <-
      g_bipc +
      geom_segment(
        data = bootstraps_bipc_summary,
        aes(xend = block, y = lo, yend = up, col = block),
        linewidth = 2, alpha = 0.6, lineend = "round"
      ) +
      geom_point(aes(col = block), size = 3)
  }


  g_bipc

}
