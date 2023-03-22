
plot_varX_varY <- function(inputs, Y, varX, varY, y_lab) {
  input_var <- get_block_data(inputs)
  blockX <- input_var$block[input_var$variable == varX]
  df <- 
    tibble(
      x = inputs[[blockX]][,varX],
      y = Y[,varY],
      Arm = ifelse(inputs[["Arm"]]$Placebo, "Placebo","LACTIN-V")
    ) 
  
  
  ggplot(df, aes(x = x, y = y, col = Arm)) +
    geom_point(alpha = 0.5, size = 0.75) +
    geom_smooth(method = "lm", formula = "y ~ x", aes(fill = Arm)) +
    scale_color_manual(values = c("steelblue2","coral")) +
    scale_fill_manual(values = c("steelblue2","coral")) +
    xlab(varX) + ylab(y_lab)
  
}