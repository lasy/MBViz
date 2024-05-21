
#' Plot the variables loading
#'
#' @param res the output of `ade4::mbpls` or `ade4::mbpcaiv`
#' @param xaxis an `integer` specifying which latent variable should be plotted on the x-axis. Default is 1.
#' @param yaxis an `integer` specifying which latent variable should be plotted on the y-axis. Default is 1.
#' @param scale_axes how the aspect ratio between the x-axis and y-axis should be computed (default is "fixed", other option is "eig"). XXX explain better XXX
#' @param VIP a `logical` indicating whether to only include the important variables (`TRUE`) or all variables (`FALSE`).
#'            Default is `FALSE`.
#'            Variables are deemed important if the ratio between their `vipc` and their "reference" `vipc` is larger than 1.
#' @param boot the output of `ade::boot` or `packMBPLSDA::boot` from the corresponding model (`res`).
#'        If provided, variables are deemed important if the lower bound of their 95% confidence interval is larger than 1/20th of the reference VIPC.
#'
#' @return a `ggplot2` object
#' @export
#'
#' @import  ggplot2
#' @importFrom dplyr as_tibble mutate left_join join_by
plot_mtb_Xloadings <- function(res, xaxis = 1, yaxis = 2, scale_axes = "eig", VIP = FALSE, boot = NULL){

  faX <- get_mtb_loadings(res, cut_at_nf = FALSE)

  if (VIP)
    faX <-
      faX |>
      left_join(
        get_mtb_vipc(res = res, boot = boot) |>
          rename(vipc = value, vipc_lo = lo, vipc_up = up, vipc_mean = mean, vipc_sd = sd),
        by = join_by(variable, block)
      ) |>
      left_join(get_ref_vipc(res), by = join_by(variable, block)) |>
      mutate(
        rel_imp = vipc / vipc_ref,
        median_rel_imp = ifelse(is.na(median), rel_imp, median / vipc_ref)
      ) |>
      filter((rel_imp > 1) | (median_rel_imp > 1), (rel_imp > 0.8) & (median_rel_imp > 0.8))


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
    xlab(str_c('Axis ',xaxis)) +
    ylab(str_c("Axis ", yaxis))

}

#' Retrieve the variables loading as tibble
#'
#' @param res the output of `ade4::mbpls`, `ade4::mbpcaiv`, or `packMBPLSDA::mbplsda`
#' @param cut_at_nf a `logical` indicating whether to only include the first `res$nf` latent dimensions (`TRUE`) or all available latent dimensions (`FALSE`). Default is `TRUE`.
#' @return a `tibble` object
#' @export
#'
#' @importFrom dplyr as_tibble mutate left_join join_by
get_mtb_loadings <- function(res, cut_at_nf = TRUE){

  input_var <- blocks_and_variables(res)

  if (cut_at_nf) j <- 1:res$nf else j <- 1:ncol(res$faX)

  faX <-
    res$faX[,j] |>
    as_tibble()  |>
    mutate(variable = rownames(res$faX)) |>
    left_join(input_var, by = join_by(variable))

  faX
}
