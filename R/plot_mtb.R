
#' Plot results from a `ade4::mbpls` or `ade4::mbpcaiv` output
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param input_var a `data.frame` xxxx
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX o)
#'
#' @return a `patchwork` of `ggplot2` plots.
#' @export
#'
#' @import ggplot2
#' @import patchwork
plot_mtb <-
  function(res, xaxis = 1, yaxis = 2, scale_axes = "fixed", samples_color = NULL, block_colors = NULL) {
    g_Y <-
      plot_mtb_Y(
        res = res,
        xaxis = xaxis, yaxis = yaxis, scale_axes = scale_axes,
        samples_color = samples_color
        ) + guides(alpha = "none")
    g_lX <-
      plot_mtb_lX(
        res = res,
        xaxis = xaxis, yaxis = yaxis, scale_axes = scale_axes,
        samples_color = samples_color
        ) + guides(alpha = "none", col = "none")

    g_eig <-
      plot_mtb_eig(res = res, xaxis = xaxis, yaxis = yaxis)

    g_Xloadings <-
      plot_mtb_Xloadings(res = res, xaxis = xaxis, yaxis = yaxis, scale_axes = scale_axes) +
      guides(col = "none")

    g_cov <-
      plot_mtb_cov(res = res, xaxis, yaxis, scale_axes = scale_axes)

    if (!is.null(block_colors)) {
      g_Xloadings <- g_Xloadings + scale_color_manual(values = block_colors)
      g_cov <- g_cov + scale_color_manual(values = block_colors)
    }

    design <- "
  ACCC
  BDDD
  EEEE
  FFFF
  "

    design <- "
  ACCC
  BCCC
  EEFF
  EEFF
  "

    g_eig +
      # g_Y +
      g_lX +
      g_cov +
      g_Xloadings +
      plot_layout(design = design, height = c(2,2,1,4))
  }
