
plot_mtb_lX <- function(res, Y, xaxis = 1, yaxis = 2, scale_axes = "eig") {
  
  Y_long <- 
    Y %>% 
    mutate(USUBJID = rownames(Y)) %>% 
    pivot_longer(cols = -USUBJID, names_to = "topic", values_to = "prop")
  
  Xrow <- 
    res$lX %>%  # global components of the explanatory tables
    as_tibble() %>% 
    mutate(USUBJID = rownames(res$lX)) %>% 
    left_join(., Y_long, by = join_by(USUBJID), multiple = "all")
  
  topics <- Y_long$topic %>% unique() %>% sort()
  
  Xrow$xaxis <- Xrow[, str_c('Ax',xaxis)] %>% unlist()
  Xrow$yaxis <- Xrow[, str_c('Ax',yaxis)] %>% unlist()
  
  
  if (scale_axes == "eig") {
    asp <- sqrt(res$eig[yaxis] / res$eig[xaxis])
  } else {
    asp <- 1
  }
  
  ggplot(Xrow, aes(x = xaxis, y = yaxis)) +
    geom_vline(xintercept = 0, col = "gray50") +
    geom_hline(yintercept = 0, col = "gray50") +
    coord_fixed(ratio = asp) +
    geom_point(aes(col = topic, alpha = prop)) +
    scale_alpha(range = c(0, 1)) +
    scale_color_manual(
      breaks = topics, 
      values = get_topic_colors(topics),
      labels = get_topic_labels(topics)
      ) +
    xlab(str_c('Axis ',xaxis)) +
    ylab(str_c("Axis ",yaxis))
  
}  
