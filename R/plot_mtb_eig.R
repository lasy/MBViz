
#' Plots the eigenvalues and the cummulative projected inertia
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis (optional) an `integer` specifying which latent variable to highlight. Default is `NULL`
#' @param yaxis (optional) an `integer` specifying which latent variable to highlight. Default is `NULL`
#' @param annot (optional) a `logical` specifying if an annotation with the total
#' cumulative inertia should be added to the plot. Default is `TRUE`.
#' @param title (optional) a `character` for the plot title. Default is `NULL`.
#'
#'
#' @return a `ggplot2` object
#' @export
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_c
#' @import ggplot2
#' @import patchwork
plot_mtb_eig <- function(res, xaxis = NULL, yaxis = NULL, annot = TRUE, title = NULL){

  eigenvalues <-
    get_mtb_eig(res) %>%
    mutate(
      axis_cat =
        case_when(
          Ax %in% c(xaxis, yaxis) ~ "axes shown",
          Ax %in% 1:res$nf ~ "latent axes",
          TRUE ~ "other axes"
        )
    )

  color_breaks <- c("axes shown", "latent axes", "other axes")
  color_values <- c("black", "gray35", "gray70")
  xlim <- c(0.5, nrow(eigenvalues) + 0.5)

  g_eig <-
    ggplot(eigenvalues, aes(x = Ax, y = eig, fill = axis_cat)) +
    geom_bar(stat = "identity") +
    ylab("Eigenvalues") +
    guides(fill = "none") +
    scale_fill_manual(breaks = color_breaks, values = color_values) +
    xlab("Latent component") +
    xlim(xlim)

  g_cum_var <-
    ggplot(eigenvalues, aes(x = Ax, y = 100*cumvar, fill = axis_cat)) +
    geom_bar(stat = "identity") +
    ylab("Cummulative\nprojected inertia (%)") +
    guides(fill = "none")  +
    scale_fill_manual(breaks = color_breaks, values = color_values) +
    xlab("Latent component") +
    xlim(xlim)

  if (annot) {

    tot_cum_var <- 100*eigenvalues$cumvar[res$nf]
    g_cum_var <-
      g_cum_var +
      annotate(
        geom = "segment",
        x  = -Inf, xend = res$nf + 0.5,
        y = tot_cum_var, yend = tot_cum_var,
        col = "gray55", linetype = "21")  +
      annotate(
        geom = "text",
        x = 0.51, y = tot_cum_var + 2, label = str_c(round(tot_cum_var),"%"),
        hjust = 0, vjust = 0, col = "gray30"
      )
  }

  if (!is.null(title)) {
    g_eig <- g_eig + ggtitle(title)
  }

  g_eig + g_cum_var

}
