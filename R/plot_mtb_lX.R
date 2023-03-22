
#' Plot the sample projections onto the latent space
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX
#' @param samples_color (optional) a `vector` or a `matrix` specifying how samples should be colored. XXX give more explanations XXX
#'
#' @return a `ggplot2` object
#' @export
#'
#' @import ggplot2
#' @importFrom dplyr as_tibble mutate left_join join_by
#' @importFrom tidyr pivot_longer
plot_mtb_lX <- function(res, xaxis = 1, yaxis = 2, scale_axes = "eig", samples_color = NULL) {

  Xrow <-
    res$lX %>%  # global components of the explanatory tables
    as_tibble() %>%
    mutate(sampleID = rownames(res$lX))

  Xrow <- Xrow %>% .add_colors(., samples_color)

  Xrow$xaxis <- Xrow[, str_c('Ax',xaxis)] %>% unlist()
  Xrow$yaxis <- Xrow[, str_c('Ax',yaxis)] %>% unlist()

  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }

  g <-
    ggplot(Xrow, aes(x = xaxis, y = yaxis)) +
    geom_vline(xintercept = 0, col = "gray50") +
    geom_hline(yintercept = 0, col = "gray50") +
    coord_fixed(ratio = asp) +
    xlab(str_c('Axis ',xaxis)) +
    ylab(str_c("Axis ",yaxis))

  if (!is.null(samples_color)) {
    if (all(Xrow$value == 1)) {
      g <-
        g +
        geom_point(aes(col = variable))
    } else {
      g <-
        g +
        geom_point(aes(col = variable, alpha = value)) +
        scale_alpha(range = c(0, 1))
    }
  } else {
    g <- g + geom_point()
  }

  g

}
