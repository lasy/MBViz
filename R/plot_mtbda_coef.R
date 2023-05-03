
plot_mtbda_coef <- function(res, boot = NULL, Yvar, format = "long") {

  coefs <- get_mtbda_coef(res = res, boot = boot)

  if (!("lo" %in% colnames(coefs))) coefs <-  coefs %>% mutate(lo = value, up = value)
  coefs <- coefs %>% mutate(CI_excludes_0 = ((lo*up) > 0)*1)

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

