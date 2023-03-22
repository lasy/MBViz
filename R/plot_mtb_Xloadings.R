
#' Plot the projections of the input variable onto the latent space
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param input_var a `data.frame` xxxx
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX
#'
#' @return a `ggplot2` object
#' @export
#'
#' @import  ggplot2
#' @importFrom dplyr as_tibble mutate left_join join_by
plot_mtb_Xloadings <- function(res, input_var, xaxis = 1, yaxis = 2, scale_axes = "eig"){

  faX <-
    res$faX %>%
    as_tibble() %>%
    mutate(variable = rownames(res$faX)) %>%
    left_join(input_var, by = join_by(variable))


  faX$xaxis <- faX[, str_c('Ax',xaxis)] %>% unlist()
  faX$yaxis <- faX[, str_c('Ax',yaxis)] %>% unlist()


  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }


  ggplot(faX, aes(x = xaxis, y = yaxis, col = block)) +
    coord_fixed(ratio = asp) +
    geom_vline(xintercept = 0, col = "gray50") +
    geom_hline(yintercept = 0, col = "gray50") +
    geom_segment(
      aes(xend = 0, yend = 0),
      arrow = arrow(ends = "first", type = "closed", length = unit(10,"pt"))
    ) +
    geom_label_repel(aes(label = variable), min.segment.length = 1, size = 3)  +
    xlab(str_c('Ax',xaxis)) +
    ylab(str_c("Ax", yaxis))

}
