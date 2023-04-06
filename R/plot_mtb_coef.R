
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
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble as_tibble mutate row_number group_by summarize left_join join_by arrange n
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_wrap
#' @importFrom stats quantile
#' @importFrom forcats fct_rev
plot_mtb_coef <- function(res, boot = NULL, Y_var = NULL, CI = 0.95, format = "long", max_coef_value = NULL){

  format <- match.arg(format, choices = c("long", "wide"))
  input_var <- blocks_and_variables(res)
  all_Y_vars <- names(res$XYcoef)
  if (is.null(Y_var)) Yvars <- all_Y_vars
  else if (all(is.character(Y_var))) {
    if (all(Y_var %in% all_Y_vars))  Yvars <- Y_var
    else stop("Y_var must only include variables in the Y block\n")
  }
  else if (all(as.integer(Y_var) == Y_var)) {
    if (all(Y_var %in% 1:length(all_Y_vars))) Yvars <- all_Y_vars[Y_var]
    else stop("Y_var indices must be in 1:ncol(Y)\n")
  } else stop("Y_var must be a character or integer (vector).")


  nf <- ifelse(is.null(boot), res$nf, which.min(abs(boot$XYcoef[[1]]$obs[1] - res$XYcoef[[1]][1,])))

  coefs <-
    purrr::map_dfr(
      .x = Yvars,
      .f = function(Yvar, res) {
        tibble(
          variable = rownames(res$XYcoef[[Yvar]]),
          value = res$XYcoef[[Yvar]][, nf],
          Yvar = Yvar
        )
      },
      res = res
    ) %>%
    mutate(
      variable = variable %>% factor(., levels = input_var$variable),
      Yvar = Yvar %>% factor(., levels = all_Y_vars)
      ) %>%
    left_join(input_var, by = join_by(variable))

  if (!is.null(boot)) {
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
      ) %>%
      mutate(
        variable = variable %>% factor(., levels = input_var$variable),
        Yvar = Yvar %>% factor(., levels = all_Y_vars)
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
      mutate(CI_excludes_0 = ((lo*up) > 0)*1)

    coefs <- coefs %>% left_join(boot_summary, by = join_by(variable, Yvar))
  } else {
    coefs <- coefs %>% mutate(CI_excludes_0 = 1, up = value, lo = value)
  }

  if (!is.null(max_coef_value)) {
    coefs <-
      coefs %>%
      mutate(
        value = ifelse(value > max_coef_value, NA, value),
        up = up %>% pmin(., max_coef_value)
        )
  }

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
