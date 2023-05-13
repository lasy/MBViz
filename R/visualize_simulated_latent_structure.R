
#' Plot the simulated latent structures
#'
#' @param A the output of `simulate_Xs_Y`
#'
#' @return a `ggplot2` object
#' @export
#'
visualize_simulated_latent_structure <- function(A){
  if (ncol(A$Bxx) != ncol(A$Bxy))
    stop("Bxx and Bxy must have the same number of columns")
  if (nrow(A$Bxy) != nrow(A$By))
    stop("Bxy and By must have the same number of rows")

  if (is.null(A$p)) p <- ncol(A$Bxx) else p <- A$p

  low_col <- "indianred1"
  high_col <- "dodgerblue1"

  g_Bxy <-
    ggplot(A$Bxy %>% to_long() %>% to_blocks(., p = p),
           aes(y = l, x = var, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = low_col, high = high_col, breaks = -1:1, limits = c(-1.01,1.01)) +
    facet_grid(. ~ Xi, scales = "free", space = "free", labeller = label_both) +
    xlab("variables") +
    ylab("lY") +
    ggtitle("Explanatory\nvariables")

  g_By <-
    ggplot(A$By %>% to_long, aes(y = l, x = var, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = low_col, high = high_col, breaks = -1:1, limits = c(-1.01,1.01)) +
    xlab("variables") +
    ylab("lY") +
    facet_grid(. ~ "Y") +
    ggtitle("Response\nvariables")

  g_Bxx <-
    ggplot(A$Bxx %>% to_long() %>% to_blocks(., p = p),
           aes(y = l, x = var, fill = value)) +
    geom_tile() +
    scale_fill_gradient2(low = low_col, high = high_col, breaks = -1:1, limits = c(-1.01,1.01)) +
    facet_grid(. ~ Xi, scales = "free", space = "free", labeller = label_both) +
    xlab("variables") +
    ylab("lX")


  patchw <- g_Bxy + g_By

  if (all(A$Bxx == 0)) {
    layout_nrow <- 1; layout_heights <- 1
  } else {
    layout_nrow <- 2; layout_heights <- c(nrow(A$By), nrow(A$Bxx))
    patchw <- patchw + g_Bxx + patchwork::plot_spacer()
  }

  patchw +
    plot_layout(
      nrow = layout_nrow, ncol = 2, byrow = TRUE, guides = "collect",
      widths = c(sum(p), ncol(A$By)), heights = layout_heights
    )

}

to_long <- function(B){
  B %>%
    set_colnames(1:ncol(B)) %>%
    as_tibble() %>%
    mutate(l = row_number() %>% as.factor()) %>%
    pivot_longer(-l, names_to = "var", values_to = "value") %>%
    mutate(
      var = var %>% as.integer() %>% as.factor(),
      value = value %>% as.double())
}

to_blocks <- function(Bl, p) {
  Bl %>%
    left_join(
      .,
      tibble(
        var = 1:sum(p) %>% as.factor(),
        Xi = rep(1:length(p), p) %>% as.factor()
      ),
      by = "var"
    )
}
