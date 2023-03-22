
plot_mtb_cov <- function(res, input_var, xaxis = 1, yaxis = 2, scale_axes = "eig") {
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
    ylab(str_c("Axis ", yaxis)) +
    scale_color_manual(
      breaks = cov$block,
      values = get_block_colors(cov$block)
    )
  
}
