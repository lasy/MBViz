
#' Plots the observed (and bootstrapped) cumulative Variable Importance in Projection (VIPC)
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`)
#' @param boot (optional) the output of `ade4::randboot`
#' @param CI (optional) The width of the confidence interval that should be displayed. Default is 0.95.
#'
#' @return a `ggplot2` object
#' @export
#'
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate left_join join_by arrange
#' @importFrom stringr str_wrap
#' @import ggplot2
plot_mtb_vipc <- function(res, boot, CI = 0.95){

  input_var <- blocks_and_variables(res)
  if (class(res) == "mbplsda") {
    nf <- ifelse(is.null(boot), res$nf, boot$call$optdim)
  } else {
    nf <- ifelse(is.null(boot), res$nf, which.min(abs(boot$bipc$obs[1] - res$bipc[1,])))
  }
  vipc <-
    tibble(variable = rownames(res$vipc), value = res$vipc[,nf]) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable %>% levels())
    ) %>%
    left_join(input_var, by = join_by(variable))

  if (!is.null(boot)) {
    if (class(boot) == "boot_mbplsda") {
      vipc <-
        vipc %>%
        left_join(
          boot$vipc %>%
            dplyr::select(variables, block, Q2.5, Q97.5) %>%
            dplyr::rename(variable = variables, lo = Q2.5, up = Q97.5) %>%
            dplyr::mutate(
              variable = variable %>% factor(., levels = input_var$variable),
              block = block %>% factor(., levels = input_var$block %>% levels())
              ),
          by = join_by(variable, block)
        )
    } else {

    bootstraps_vipc <-
      boot$vipc$boot %>%
      t() %>%
      set_colnames(1:nrow(boot$vipc$boot)) %>%
      as_tibble() %>%
      mutate(
        variable =
          colnames(boot$vipc$boot) %>%
          factor(., levels = input_var$variable %>% levels())
      ) %>%
      pivot_longer(
        cols = -variable,
        names_to = "b",
        values_to = "value"
      )

    bootstraps_vipc_summary <-
      bootstraps_vipc %>%
      group_by(variable) %>%
      summarize(
        lo = quantile(value, p = (1-CI)/2),
        up = quantile(value, p = CI + (1-CI)/2),
        .groups = "drop"
      )

    vipc <- vipc %>% left_join(bootstraps_vipc_summary, by = join_by(variable))
    }
  } else {
    vipc <- vipc %>% mutate(lo = 0, up = value)
  }



  g_vipc <-
    ggplot(vipc, aes(x = variable, y = value, col = block)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1/length(unique(vipc$variable)),
               linetype = 3, col = "gray70") +
    geom_segment(
      aes(xend = variable, y = lo, yend = up),
      lineend = "round",
      alpha = ifelse(is.null(boot), 1, 0.5),
      linewidth = ifelse(is.null(boot), 0.5, 2)
      ) +
#     geom_boxplot(aes(col = block, fill = block), alpha = 0.5) +
    geom_point(size = 3) +
    expand_limits(y = 0, ) +
    facet_grid(. ~ pretty_block, scales = "free", space = "free") +
    guides(fill = "none", col = "none") +
    xlab("") +
    ylab("Cummulative\nVariable Importance") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g_vipc
}



# obs <-
#   tibble(variable = names(boot$vipc$obs), value = boot$vipc$obs) %>%
#   mutate(
#     variable =
#       variable %>%
#       factor(., levels = input_var$variable %>% levels())
#   ) %>%
#   left_join(input_var, by = join_by(variable)) %>%
#   arrange(block) %>%
#   mutate(
#     pretty_block = str_wrap(block, width = 12),
#     pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
#   )
