
#' Plots the bootstraped and observed Block Importance
#'
#' @param boot the output of `                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           `
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`) (the input of)
#'
#' @return a `ggplot2` object
#' @export
#'
#' @importFrom magrittr %>% set_colnames
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr as_tibble mutate left_join join_by arrange
#' @importFrom stringr str_wrap
#' @import ggplot2
plot_mtb_vipc <- function(boot, res){

  input_var <- blocks_and_variables(res)

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

  bootstraps_vipc <-
    bootstraps_vipc %>%
    left_join(input_var, by = join_by(variable)) %>%
    arrange(block) %>%
    mutate(
      pretty_block = str_wrap(block, width = 12),
      pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
    )

  obs <-
    tibble(variable = names(boot$vipc$obs), value = boot$vipc$obs) %>%
    mutate(
      variable =
        variable %>%
        factor(., levels = input_var$variable %>% levels())
    ) %>%
    left_join(input_var, by = join_by(variable)) %>%
    arrange(block) %>%
    mutate(
      pretty_block = str_wrap(block, width = 12),
      pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
    )

  g_vipc <-
    ggplot(bootstraps_vipc,
           aes(x = variable, y = value)) +
    geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1/length(unique(bootstraps_vipc$variable)),
               linetype = 3, col = "gray70") +
    geom_boxplot(aes(col = block, fill = block), alpha = 0.5) +
    geom_point(data = obs) +
    expand_limits(y = 0, ) +
    facet_grid(. ~ pretty_block, scales = "free", space = "free") +
    guides(fill = "none", col = "none") +
    xlab("") +
    ylab("Cummulative\nVariable Importance") +
    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g_vipc
}
