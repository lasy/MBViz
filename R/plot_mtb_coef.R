
#' Plots the coefficients of associations between the explanatory and response variables
#'
#' @param boot the output of `ade4::randboot`
#' @param Y_var (optional) a `character` specifying the names of selected response variables
#' if one desires to display the coefficients for a subset of variables.
#' @param input_var a `data.frame` xxxx
#' @param CI The width of the confidence interval that should be displayed. Default is 0.95.
#' @param format "long" or "wide". a `character` specifying if the coefficients should be displayed horizontally or vertically. (default = "long")
#' @param max_coef_value an optional upper limit on the coefficient value displayed (to crop the axes).
#'
#' @return a `ggplot2` object
#' @export
#' @import ggplot2
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble as_tibble mutate row_number group_by summarize left_join join_by arrange
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_wrap
#' @importFrom stats quantile
plot_mtb_coef <- function(boot, Y_var = NULL, input_var, CI = 0.95, format = "long", max_coef_value = NULL){

  format <- match.arg(format, choices = c("long", "wide"))
  if (is.null(Y_var))  Yvars <- names(boot$XYcoef) else Yvars <- Y_var
  boot_res <-
    purrr::map_dfr(
      .x = Yvars,
      .f = function(Yvar, boot) {
        boot$XYcoef[[Yvar]]$boot %>%
          as_tibble() %>%
          mutate(i = row_number()) %>%
          pivot_longer(
            cols = -i,
            names_to = "variable",
            values_to = "value"
          ) %>%
          mutate(Yvar = Yvar)
      },
      boot = boot
    )

  boot_summary <-
    boot_res %>%
    group_by(variable, Yvar) %>%
    summarize(
      lo = quantile(value, p = (1-CI)/2),
      up = quantile(value, p = CI + (1-CI)/2),
      p = abs(sum(value > 0) - sum(value < 0))/n(),
      .groups = "drop"
    ) %>%
    mutate(CI_excludes_0 = ((lo*up)>0)*1)  %>%
    left_join(., input_var, by = join_by(variable)) %>%
    arrange(block) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable),
      pretty_block = str_wrap(block, width = 12),
      pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
    )


  obs_coef <-
    purrr::map_dfr(
      .x = Yvars,
      .f = function(Yvar, boot) {
        tibble(
          variable = names(boot$XYcoef[[Yvar]]$obs),
          value = boot$XYcoef[[Yvar]]$obs,
          Yvar = Yvar
        )
      },
      boot = boot
    )

  obs_coef <-
    obs_coef %>%
    left_join(input_var, by = join_by(variable)) %>%
    left_join(boot_summary, by = join_by(Yvar, variable, block)) %>%
    arrange(block) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable),
      pretty_block = str_wrap(block, width = 12),
      pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
    )

  block_names <- boot_summary$block %>% unique()


  if (!is.null(max_coef_value)) {
    boot_summary <- boot_summary %>% mutate(up = up %>% pmin(., max_coef_value))
    obs_coef <- obs_coef %>% mutate(value = ifelse(value > max_coef_value, NA, value))
  }


  if (format == "long") {
    boot_summary <- boot_summary %>% mutate(variable = variable %>% fct_rev())
    obs_coef <- obs_coef %>% mutate(variable = variable %>% fct_rev())
  }

  # geom_boxplot(data = boot_res, aes(y = value, col = block, fill = block), alpha = 0.5) +

  g <-
    ggplot(boot_summary, aes(x = variable, col = block, alpha = CI_excludes_0)) + # , linewidth = p, size = p
    geom_hline(yintercept = 0, col = "gray50") +
    geom_segment(aes(xend = variable, y = lo, yend = up), lineend = "round") +
    geom_point(data = obs_coef, aes(y = value)) +
    xlab("") +
    ylab("coefficient") +
    scale_alpha(range = c(0.3, 1)) +
    # scale_linewidth(range = c(1,2)) +
    # scale_size(range = c(1,3)) +
    guides(fill = "none", col = "none", alpha = "none") + # , linewidth = "none", size = "none"
    theme(
      strip.text = element_text(color = "black"),
      strip.text.y = element_text(angle = 0, hjust = 0)
      ) +
    scale_color_manual(
      breaks = block_names,
      values = get_block_colors(block_names)
    )

  if (format == "wide") {
    g <-
      g +
      facet_grid(Yvar ~ pretty_block, scales = "free_x", space = "free_x") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    g <-
      g +
      coord_flip() +
      facet_grid(pretty_block ~ Yvar, scales = "free_y", space = "free_y")
  }

  g

}
