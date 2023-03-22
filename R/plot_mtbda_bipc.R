
plot_mtbda_bipc <- function(boot_res) {
  
  boot_res$bipc %>% 
    mutate(blocks = blocks %>% factor(., levels = unique(blocks))) %>% 
    ggplot(., aes(x = blocks, y = median, col = blocks)) + 
    geom_hline(yintercept = 1/nrow(boot_res$bipc), linetype = 3) +
    geom_hline(yintercept = 0) +
    geom_segment(aes(xend = blocks, y = `Q2.5`, yend = `Q97.5`), alpha = 0.6, linewidth = 2, lineend = "round")  +
    geom_point(size = 3) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(col = "none") + xlab("") + 
    ylab("Cummulative\nBlock Importance") +
    scale_color_manual(
      breaks = boot_res$bipc$blocks,
      values = boot_res$bipc$blocks %>% get_block_colors()
    )
  
  
}