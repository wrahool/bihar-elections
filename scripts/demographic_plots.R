library(tidyverse)
library(patchwork)

make_plot <- function(f) {
  # Read the data (replace 'your_file.csv' with your actual filename)
  # We use check.names = FALSE to preserve spaces if they exist in headers
  df <- read_csv(f)
  
  # Dynamic Pre-processing
  # Identify column names by position
  x_label <- colnames(df)[1]
  df[[x_label]] <- factor(df[[x_label]], levels = unique(df[[x_label]]))
  
  group1_label <- colnames(df)[2]
  group2_label <- colnames(df)[3]
  
  # Reshape the data from 'Wide' to 'Long' format
  # We pivot columns 2 and 3 based on their index
  df_long <- df %>%
    pivot_longer(
      cols = c(2, 3), 
      names_to = "Measurement_Type", 
      values_to = "Value"
    )
  
  # Generate the Plot
  p <- ggplot(df_long, aes(x = .data[[x_label]], y = Value, fill = Measurement_Type)) +
    # 'dodge' creates the side-by-side grouped effect
    geom_bar(stat = "identity", position = position_dodge()) +
    # Applying the requested Orange and Blue color scheme
    scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) + 
    labs(
      x = x_label,
      y = "% composition",
      fill = "Category"
    ) +
    theme_minimal()
  
  return(p)
}

caste_fig <- make_plot("data/demographics/caste.csv")
gender_fig <- make_plot("data/demographics/gender.csv")
age_fig <- make_plot("data/demographics/age.csv")
religion_fig <- make_plot("data/demographics/religion.csv")
votechoice_fig <- make_plot("data/demographics/vote choice.csv")

combined_plot <- (caste_fig + gender_fig + age_fig + religion_fig + votechoice_fig) + 
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = 'A') & 
  theme(legend.position = "bottom")

# Display
combined_plot

ggsave(
  filename = "figures/demographic plots.svg", 
  plot = combined_plot, 
  device = "svg",
  width = 9,       # Adjust width as needed
  height = 10,      # Adjust height as needed
  units = "in"
)
