
plot_mtb_eig <- function(res, xaxis = 1, yaxis = 2){
  
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
  
  g_eig <- 
    ggplot(eigenvalues, aes(x = Ax, y = eig, fill = axis_cat)) +
    geom_bar(stat = "identity") +
    ylab("Eigenvalues") +
    guides(fill = "none") +
    scale_fill_manual(values = c("black", "gray55", "gray90")) +
    xlab("Axes")
  
  g_cum_var <- 
    ggplot(eigenvalues, aes(x = Ax, y = 100*cumvar, fill = axis_cat)) +
    geom_bar(stat = "identity") +
    ylab("Cummulative\nprojected inertia (%)") +
    guides(fill = "none")  +
    scale_fill_manual(values = c("black", "gray55", "gray90")) +
    xlab("Axes")
  
  g_eig + g_cum_var
  
}