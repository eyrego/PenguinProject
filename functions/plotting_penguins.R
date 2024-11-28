## ---------------------------
##
## Script name: plotting_penguins.r
##
## Purpose of script: 
##      # Functions for plotting publication quality figures and saving those figures.
##
## Author: Dr. Lydia France and Oliver Eyre
##
## Date Created: 2024-11-28
##
##
## ---------------------------
##
## Notes: A file to go alongside the reproducible figures lessons from MT weeks 
## 1-4, storing functions to clean up the penguins dataset found in palmerpenguins.
##   
##
## ---------------------------

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

#Function to save png plots:
save_flipper_plot_png <- function(boxplot, 
                                  filename, size, res, scaling){
  agg_png(filename, width   =  size, 
          height  =  size, 
          units   =  "cm", 
          res     =  res, 
          scaling =  scaling)
  print(boxplot)
  dev.off()
}

#The png plots we made
flipper_boxplot <- plot_boxplot(penguins_clean, 
                                species, flipper_length_mm, 
                                "Species", "Flipper Length (mm)", 
                                species_colours)

save_flipper_plot_png(flipper_boxplot, 
                      here("figures", "flipper_boxplot_report.png"), 
                      size = 20, res = 300, scaling = 2)

save_flipper_plot_png(flipper_boxplot, 
                      here("figures", "flipper_boxplot_poster.png"), 
                      size = 40, res = 300, scaling = 4)

save_flipper_plot_png(flipper_boxplot, 
                      here("figures", "flipper_boxplot_powerpoint.png"), 
                      size = 20, res = 300, scaling = 3)

#function to save svg plots
save_flipper_plot_svg <- function(boxplot, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  print(boxplot)
  dev.off()
}

#The svg plots we made
flipper_boxplot <- plot_boxplot(penguins_clean, 
                                species, flipper_length_mm, 
                                "Species", "Flipper Length (mm)", 
                                species_colours)

save_flipper_plot_svg(flipper_boxplot, 
                      here("figures", "flipper_boxplot_report.svg"), 
                      size = 20, scaling = 2)

save_flipper_plot_svg(flipper_boxplot, 
                      here("figures", "flipper_boxplot_poster.svg"), 
                      size = 40, scaling = 4)

save_flipper_plot_svg(flipper_boxplot, 
                      here("figures", "flipper_boxplot_powerpoint.svg"), 
                      size = 20, scaling = 3)

#We are so done woo