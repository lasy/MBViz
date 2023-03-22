
plot_mtb_blocks <- function(input_var, output_var){
  g_input <- .plot_mtb_blocks(input_var) + ggtitle("Inputs")
  g_output <- .plot_mtb_blocks(output_var) + ggtitle("Output")
  g_input + g_output + plot_layout(widths = c(nrow(input_var), nrow(output_var)))
}
.plot_mtb_blocks <- function(input_var) {
  block_names <- input_var$block %>% unique()
  ggplot(
    input_var %>% 
      mutate(
        pretty_block = str_wrap(block, width = 12),
        pretty_block = pretty_block %>% factor(., levels = unique(pretty_block))
      ), 
    aes(x = variable, y = 1)) +
    geom_tile(aes(fill = block), col = "white", alpha = 0.2) +
    geom_text(aes(label = variable, col = block), angle = 90) +
    facet_grid(. ~ pretty_block, scales = "free", space = "free") +
    scale_x_discrete("", breaks = NULL) +
    scale_y_continuous("", breaks = NULL) +
    guides(col = "none", fill = "none") +
    scale_fill_manual(breaks = block_names, values = get_block_colors(block_names))+
    scale_color_manual(breaks = block_names, values = get_block_colors(block_names))+
    theme(
      strip.text = element_text(color = "black")
    )
  
}

