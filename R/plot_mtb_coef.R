
#' Plots the coefficients of associations between the explanatory and response variables
#'
#' @param res the output of `ade4::mbpls` (or `ade4::mbpcaiv`) (the input of `ade4::randboot`)
#' @param boot (optional) the output of `ade4::randboot` to display the confidence intervals
#' @param Y_var (optional) a `integer` or a `character` specifying the indices
#' or names of selected response variables if one desires to display the
#' coefficients for a subset of variables.
#' @param CI (optional) The width of the confidence interval that should be displayed. Default is 0.95.
#' @param format "long" or "wide". a `character` specifying if the coefficients should be displayed horizontally or vertically. (default = "long")
#' @param max_coef_value an optional upper limit on the coefficient value displayed (to crop the axes).
#'
#' @return a `ggplot2` object
#' @export
#' @import ggplot2
#' @importFrom dplyr tibble as_tibble mutate row_number group_by summarize left_join join_by arrange n
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_wrap
#' @importFrom stats quantile
#' @importFrom forcats fct_rev
plot_mtb_coef <- function(res, boot = NULL, Y_var = NULL, CI = 0.95, format = "long", max_coef_value = NULL){


  coefs <- get_mtb_coef(res = res, boot = boot, Y_var = Y_var, CI = CI)

  if (!("lo" %in% colnames(coefs))) coefs <-  coefs %>% mutate(lo = value, up = value)
  coefs <- coefs %>% mutate(CI_excludes_0 = ((lo*up) > 0)*1)

  if (!is.null(max_coef_value)) {
    coefs <-
      coefs %>%
      mutate(
        value = ifelse(value > max_coef_value, NA, value),
        up = up %>% pmin(., max_coef_value)
        )
  }

  format <- match.arg(format, choices = c("long", "wide"))
  if (format == "long") coefs <- coefs %>% mutate(variable = variable %>% fct_rev())

  g <-
    ggplot(coefs, aes(x = variable, y = value, col = block, alpha = CI_excludes_0)) +
    geom_hline(yintercept = 0, col = "gray50") +
    geom_segment(aes(xend = variable, y = lo, yend = up), lineend = "round") +
    geom_point() +
    xlab("") +
    ylab("coefficient") +
    scale_alpha_continuous(breaks = c(0, 1), range = c(0.3, 1)) +
    guides(fill = "none", col = "none", alpha = "none") +
    theme(
      strip.text = element_text(color = "black"),
      strip.text.y = element_text(angle = 0, hjust = 0)
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



# obs_coef <-
#   purrr::map_dfr(
#     .x = Yvars,
#     .f = function(Yvar, boot) {
#       tibble(
#         variable = names(boot$XYcoef[[Yvar]]$obs),
#         value = boot$XYcoef[[Yvar]]$obs,
#         Yvar = Yvar
#       )
#     },
#     boot = boot
#   ) %>%
#   mutate(Yvar = Yvar %>% factor(., levels = all_Y_vars))
#
# obs_coef <-
#   obs_coef %>%
#   left_join(input_var, by = join_by(variable)) %>%
#   left_join(boot_summary, by = join_by(Yvar, variable, block)) %>%
#   arrange(block) %>%
#   mutate(
#     variable = variable %>% factor(., levels = input_var$variable),
#     pretty_block = str_wrap(block, width = 12),
#     pretty_block = pretty_block %>% factor(., levels = unique(pretty_block)),
#     Yvar = Yvar %>% factor(., levels = all_Y_vars)
#   )
