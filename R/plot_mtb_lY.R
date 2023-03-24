
#' Plot the projections of the response variables onto the latent space
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX
#' @param add_loadings (optional) a `logical` specifying whether the response loadings should be added to create a biplot.
#' @param samples_color (optional) a `vector` or a `matrix` specifying how samples should be colored. XXX give more explanations XXX
#'
#' @return a `ggplot2` object
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>%  divide_by
#' @importFrom dplyr as_tibble mutate
#' @importFrom ggrepel geom_label_repel
plot_mtb_lY <- function(res, xaxis = 1, yaxis = 2, scale_axes = "eig", add_loadings = TRUE, samples_color = NULL){

  Yrow <-
    res$lY %>%
    as_tibble() %>%
    mutate(sampleID = rownames(res$lY))

  Yrow <- .add_colors(Yrow, samples_color)

  Yrow$xaxis <- Yrow[, str_c("Ax", xaxis)] %>% unlist()
  Yrow$yaxis <- Yrow[, str_c("Ax", yaxis)] %>% unlist()

  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }

  g <-
    ggplot(Yrow, aes(x = xaxis, y = yaxis)) +
    geom_vline(xintercept = 0, col = "gray50") +
    geom_hline(yintercept = 0, col = "gray50") +
    coord_fixed(ratio = asp)  +
    xlab(str_c("Axis ", xaxis)) +
    ylab(str_c("Axis ", yaxis))

  if (!is.null(samples_color)) {
    g <-
      g +
      geom_point(aes(col = variable, alpha = value)) +
      scale_alpha(range = c(0, 1))
  } else {
    g <-
      g +
      geom_point()
  }

  if (add_loadings) {

    Yvec <-
      tibble(
        var = rownames(res$Yco),
        res$Yco %>% divide_by(20) %>% as_tibble()
      )
    Yvec$xaxis <- Yvec[, str_c("Ax", xaxis)] %>% unlist()
    Yvec$yaxis <- Yvec[, str_c("Ax", yaxis)] %>% unlist()

    g <-
      g  +
      geom_segment(
        data = Yvec,
        aes(xend = 0, yend = 0, col = var),
        arrow = arrow(ends = "first", type = "closed", length = unit(10,"pt"))
      )  +
      geom_label_repel(
        data = Yvec,
        aes(label = var, col = var), min.segment.length = 1
      )
  }

  if (add_loadings & is.null(samples_color)) {
    g <- g + guides(color = "none")
  }

  g

}

