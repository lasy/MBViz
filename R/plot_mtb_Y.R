

plot_mtb_Y <- function(res, Y, xaxis = 1, yaxis = 2, scale_axes = "eig", add_loadings = TRUE){
  
  Y_long <- 
    Y %>% 
    mutate(USUBJID = rownames(Y)) %>% 
    pivot_longer(cols = -USUBJID, names_to = "topic", values_to = "prop")
  
  Yrow <- 
    res$lY %>% 
    as_tibble() %>% 
    mutate(USUBJID = rownames(res$lY)) %>% 
    left_join(., Y_long, by = join_by(USUBJID), multiple = "all")
  
  topics <- Y_long$topic %>% unique() %>% sort()
  
  
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
    geom_point(aes(col = topic, alpha = prop)) +
    scale_alpha(range = c(0, 1)) +
    scale_color_manual(values = get_topic_colors(topics)) +
    guides(col = "none") +
    xlab(str_c("Axis ", xaxis)) +
    ylab(str_c("Axis ", yaxis))
  
  if (add_loadings) {
    
    Yvec <- 
      tibble(
        Y_topic = rownames(res$Yco),
        res$Yco %>% divide_by(20) %>% as_tibble()
      )
    Yvec$xaxis <- Yvec[, str_c("Ax", xaxis)] %>% unlist()
    Yvec$yaxis <- Yvec[, str_c("Ax", yaxis)] %>% unlist()
    
    g <-  
      g  +
      geom_segment(
        data = Yvec,
        aes(xend = 0, yend = 0, col = Y_topic), 
        arrow = arrow(ends = "first", type = "closed", length = unit(10,"pt"))
      )  +
      geom_label_repel(
        data = Yvec, 
        aes(label = Y_topic, col = Y_topic), min.segment.length = 1
      )
  }
  g
  
}