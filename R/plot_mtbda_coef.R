
plot_mtbda_coef <- function(boot_res, input_var, Yvar) {

  boot_res$XYcoef[[Yvar]] %>%
    dplyr::rename(variable = variables) %>%
    mutate(CI_include_0 = ((`Q2.5` * `Q97.5`) > 0) * 1) %>%
    left_join(input_var, by = join_by(variable, block)) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable %>% rev()),
      pretty_block = block %>% str_wrap(., width = 15),
      pretty_block = pretty_block %>% factor(., levels = levels(input_var$block) %>% str_wrap(., width = 15)),
      block = block %>% factor(., levels = input_var$block %>% levels())
    ) %>%
    ggplot(., aes(y = variable, x = median, col = block, alpha = CI_include_0)) +
    geom_vline(xintercept = 0) +
    geom_point() +
    geom_segment(aes(yend = variable, x = `Q2.5`, xend = `Q97.5`)) +
    scale_alpha(range = c(0.3, 1)) +
    facet_grid(pretty_block ~ ., scales = "free_y", space = "free") +
    theme(
      strip.text.y = element_text(angle = 0, hjust = 0)
    ) +
    guides(col = "none", alpha = "none") + xlab("coefficient") + ylab("")
}

