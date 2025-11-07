library(tidyverse)
library(patchwork)

plot_dv <- function(data, base_var) {
  var1 <- paste0(base_var, "NDA")
  var2 <- paste0(base_var, "UPA")
  
  long_dat <- data %>%
    select(Flooding, all_of(c(var1, var2))) %>%
    pivot_longer(cols = c(all_of(var1), all_of(var2)),
                 names_to = "Variable",
                 values_to = "Value")
  
  summary_dat <- long_dat %>%
    group_by(Variable, Flooding) %>%
    summarise(
      mean_y = mean(Value, na.rm = TRUE),
      sd_y   = sd(Value, na.rm = TRUE),
      n      = n(),
      se_y   = sd_y / sqrt(n),
      .groups = "drop"
    ) %>%
    mutate(Variable = gsub("^.*?(NDA|UPA)$", "\\1", Variable))
  
  ggplot(summary_dat, aes(x = Variable,
                          y = mean_y,
                          fill = factor(Flooding))) +
    geom_col(position = position_dodge(width = 0.8), width = 0.6) +
    geom_errorbar(aes(ymin = mean_y - se_y, ymax = mean_y + se_y),
                  position = position_dodge(width = 0.8),
                  width = 0.2) +
    labs(
      x = NULL,
      y = paste("Mean", base_var),
      fill = "Flooding"
    ) +
    theme_bw() +
    scale_x_discrete(labels = c("NDA" = "in support of NDA", "UPA" = "in support of MGB")) +
    scale_fill_manual(values = c("0" = "#1f78b4", "1" = "#b2df8a"),
                      labels = c("0" = "No Flooding", "1" = "Flooding"))
}

vars <- c("Recirculated", "Created", "Commented", "Posted", "Rallies", "Meetings")

plots <- lapply(vars, function(v) plot_dv(merged_dat, v))

# Arrange in a grid, 2 per row, with a single shared legend
final_plot <- wrap_plots(plots, ncol = 2, guides = "collect") &
  theme(legend.position = "bottom")  # put legend below

final_plot

ggsave("figures/descriptives.svg",
       plot = final_plot,
       width = 6,
       height = 6,
       dpi = 300, device = "svg")
