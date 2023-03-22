
#' Plots block covariances
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX
#'
#' @return a `ggplot2` object
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr as_tibble mutate
#' @importFrom ggrepel geom_text_repel
plot_mtb_cov <- function(res, xaxis = 1, yaxis = 2, scale_axes = "eig") {

  input_var <- blocks_and_variables(res)
  cov <-
    res$cov2 %>%
    as_tibble() %>%
    mutate(
      block =
        rownames(res$cov2) %>%
        factor(., levels = input_var$block %>% levels())
    )

  cov$xaxis <- cov[, str_c("Ax",xaxis)] %>% unlist()
  cov$yaxis <- cov[, str_c("Ax",yaxis)] %>% unlist()


  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }


  ggplot(cov, aes(x = xaxis, y = yaxis, col = block)) +
    coord_fixed(ratio = asp) +
    geom_segment(
      aes(xend = 0, yend = 0),
      arrow = arrow(ends = "first", type = "closed", length = unit(10,"pt"))
    ) +
    geom_text_repel(aes(label = block), min.segment.length = 1, force = 0.3) +
    guides(col = "none") +
    xlab(str_c("Axis ", xaxis)) +
    ylab(str_c("Axis ", yaxis))

}
