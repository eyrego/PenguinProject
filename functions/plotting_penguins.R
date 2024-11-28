species_colours <- c("Adelie" = "darkorange", 
                     "Chinstrap" = "purple", 
                     "Gentoo" = "cyan4")


plot_boxplot <- function(data, 
                         x_column, 
                         y_column, 
                         x_label, 
                         y_label, 
                         colour_mapping) {
  
  # First remove NA values
  data <- data %>%
    drop_na({{ y_column }})
  
  # Now make the plot
  ggplot(data = data, 
         aes(
           x = {{ x_column }}, 
           y = {{ y_column }}, 
           color = {{ x_column }})) +  # Use {{ }} for x and y columns
    geom_boxplot(
      width = 0.3, 
      show.legend = FALSE) +
    geom_jitter(
      alpha = 0.3,
      size = 1,
      show.legend = FALSE,
      position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(
      values = colour_mapping) +  # Use color_mapping input here
    labs(
      x = x_label, 
      y = y_label) +  # Use provided x and y labels
    theme_bw()
}

plot_boxplot(penguins_clean, 
             species, flipper_length_mm, 
             "Species", "Flipper Length (mm)", 
             species_colours)