

plot_mtb_data <- function(inputs, output){
  
  j <- 
    apply(output[[1]], 1, function(x) weighted.mean(seq_along(x), x)) %>% 
    order(., decreasing = TRUE)
  id_sort <- rownames(output[[1]])[j]
  
  inputs_long <- get_mtb_blocks(inputs)
  output_long <- get_mtb_blocks(output)
  
  max <- max(abs(c(inputs_long$value, output_long$value))) %>% ceiling()
  
  g_inputs <- 
    plot_mtb_block_data(df = inputs_long, max = max, id_sort = id_sort) + 
    ggtitle("Inputs") +
    theme(
      panel.border = element_blank(),
      strip.text = element_text(color = "black")
    )
  g_output <- 
    plot_mtb_block_data(df = output_long, max = max, id_sort = id_sort) + 
    ggtitle("Output") +
    theme(
      panel.border = element_blank(),
      strip.text = element_text(color = "black")
    )
  
  g_inputs + g_output + 
    plot_layout(
      widths = c(lengths(inputs) %>% sum, lengths(output) %>% sum), 
      guides = "collect"
    )
}


get_mtb_blocks <- function(blocks) {
  
  purrr::map_dfr(
    .x = seq_along(blocks),
    .f = function(i, blocks) {
      blocks[[i]] %>% 
        scale() %>% 
        as_tibble() %>% 
        mutate(id = rownames(blocks[[i]]), block = names(blocks)[i]) %>% 
        pivot_longer(cols = -c(id, block))
    },
    blocks = blocks
  ) %>% 
    mutate(
      block = 
        block %>% str_replace(" ","\n") %>% 
        factor(., levels = names(blocks) %>% str_replace(" ","\n")),
      name = 
        name %>% 
        factor(., levels = get_block_data(blocks)$variable %>% levels())
    )
}

plot_mtb_block_data <- function(df, max, id_sort) {
  
  ggplot(df %>% mutate(id = id %>% factor(., levels = id_sort)), 
         aes(x = name, y = id, fill = value)) +
    geom_tile() +
    xlab("") +
    scale_y_discrete("", breaks = NULL) +
    scale_fill_gradientn(
      # colors = c("coral4","coral", "gray95", "steelblue1", "steelblue4"),
      colors = c("goldenrod3","goldenrod1", "gray95", "steelblue1", "steelblue4"),
      values = c(0, 0.4, 0.5, 0.6, 1), limits = c(-max, max)
    ) +
    facet_grid(. ~ block, scales = "free_x", space = "free_x") +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
}
