
#' Plots the observed (and bootstrapped) cumulative Block Importance in Projections (BIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param show_dist (optional) a `logical` specifying if the bootstrap distribution should be shown with a boxplot.
#' Default is FALSE
#' @param CI The confidence interval that should be displayed. Default is 0.95.
#'
#' @return a `ggplot2` object
#' @export
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate select
#' @importFrom stats quantile
#' @import ggplot2
plot_mtb_bipc <- function(res, boot = NULL, show_dist = FALSE, CI = 0.95) {

  input_var <- blocks_and_variables(res)
  if (class(res) == "mbplsda") {
    nf <- ifelse(is.null(boot), res$nf, boot$call$optdim)
  } else {
    nf <- ifelse(is.null(boot), res$nf, which.min(abs(boot$bipc$obs[1] - res$bipc[1,])))
  }


  bipc <-
    tibble(block = rownames(res$bipc), value = res$bipc[,nf]) %>%
    mutate(
      block = block %>% factor(., levels = input_var$block %>% levels())
    ) %>%
    left_join(input_var %>% dplyr::select(-variable) %>% dplyr::distinct(), by = join_by(block))

  if (!is.null(boot)) {
    if (class(boot) == "boot_mbplsda") {
      bipc <-
        bipc %>%
        left_join(
          boot$bipc %>%
            dplyr::select(blocks, Q2.5, Q97.5) %>%
            dplyr::rename(block = blocks, lo = Q2.5, up = Q97.5) %>%
            dplyr::mutate(block = block %>% factor(., levels = input_var$block %>% levels())),
          by = join_by(block)
          )
    } else {

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

  bootstraps_bipc_summary <-
    bootstraps_bipc %>%
    group_by(block) %>%
    summarize(
      lo = quantile(value, p = (1-CI)/2),
      up = quantile(value, p = CI + (1-CI)/2),
      .groups = "drop"
    )

  bipc <- bipc %>% left_join(bootstraps_bipc_summary, by = join_by(block))
    }
  } else {
    bipc <- bipc %>% mutate(lo = 0, up = value)
  }

  g_bipc <-
    ggplot(bipc ,
           aes(x = block, y = value)) +
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
        aes(xend = block, y = lo, yend = up, col = block),
        alpha = ifelse(is.null(boot), 1, 0.5),
        linewidth = ifelse(is.null(boot), 0.5, 2),
        lineend = "round"
      ) +
      geom_point(aes(col = block), size = 3)
  }

  g_bipc

}
