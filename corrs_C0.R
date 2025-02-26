# Load libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)
library(ggrepel) 

# Load the dataset
data <- read.csv("C0.csv")

# Fit the mixed-effects model
lmod <- lmer(Yield ~ (1|GEN) + (1|ENV) + (1|GEN:ENV) + (1|REP:ENV), data = data)

# Extract BLUPs for GEN:ENV
blups <- coef(lmod)$`GEN:ENV`
blups_df <- as.data.frame(blups) %>%
  rownames_to_column(var = "GEN_ENV") %>%
  mutate(
    GEN = sub(":.*", "", GEN_ENV),
    ENV = sub(".*:", "", GEN_ENV)
  ) %>%
  select(GEN, ENV, `(Intercept)`) %>%
  rename(Yield = `(Intercept)`)

# Calculate environmental VPD means
env_means <- data %>%
  group_by(ENV) %>%
  summarise(VPD_mean = mean(VPD_flow, na.rm = TRUE))

# Merge BLUPs with environmental means
merged_data <- blups_df %>%
  left_join(env_means, by = "ENV")

# Separate data by Cluster (C1, C2, Other)
clustered_data <- data %>% select(GEN, Cluster) %>% distinct()
merged_data <- merged_data %>%
  left_join(clustered_data, by = "GEN")

c1_data <- merged_data %>% filter(Cluster == "C1")
c2_data <- merged_data %>% filter(Cluster == "C2")
other_data <- merged_data %>% filter(is.na(Cluster) | !Cluster %in% c("C1", "C2"))

# Define a function to plot trendlines, annotate R², and customize themes
plot_trendlines <- function(data, title_label) {
  # Calculate R² for each GEN with two decimal places
  r2_data <- data %>%
    group_by(GEN) %>%
    summarise(
      r2 = round(summary(lm(Yield ~ VPD_mean, data = .))$r.squared, 2),
      VPD_max = max(VPD_mean, na.rm = TRUE),
      Yield_pred = predict(lm(Yield ~ VPD_mean, data = .), newdata = data.frame(VPD_mean = max(VPD_mean, na.rm = TRUE))),
      .groups = "drop"
    )
  
  # Merge R² back into the data
  data <- left_join(data, r2_data, by = "GEN")
  
  ggplot(data, aes(x = VPD_mean, y = Yield, color = GEN)) +
    geom_smooth(method = "lm", se = FALSE, formula = y ~ x) +
    labs(x = "VPD (kPa)", y = "Yield (t/ha)", color = "GEN") +
    theme_bw() +
    theme(
      legend.background = element_blank(), # Remove legend background
      legend.title = element_text(size = 12, color = "black", hjust = 0.5), # Combined and corrected legend.title
      legend.text = element_text(size = 10, color = "black"),
      axis.title = element_text(size = 14, color = "black"),
      axis.text = element_text(size = 12, color = "black"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10),
      legend.position = "topright", # Initial placement; will adjust below
      legend.box.just = "right"
    ) +
    # Add R² annotation for each GEN using geom_label_repel for nice placement
    geom_label_repel(
      data = r2_data,
      aes(x = VPD_max, y = Yield_pred, label = paste0("R²=", sprintf("%.2f", r2))),
      nudge_x = 0.05 * max(data$VPD_mean, na.rm = TRUE), # Adjust horizontal position
      direction = "y",
      hjust = 0,
      size = 3,
      color = "black",
      fill = "white",
      segment.color = "grey50",
      show.legend = FALSE,
      max.overlaps = 30 # Increased max.overlaps to handle potential label overlap
    ) +
    # Add plot label (A, B, C)
    annotate(
      "text",
      x = -Inf,
      y = Inf,
      label = title_label,
      hjust = -0.1,
      vjust = 1.5,
      size = 6,
      fontface = "bold",
      color = "black"
    ) +
    # Adjust legend position to be inside the plot area
    guides(color = guide_legend(override.aes = list(fill = NA))) +
    theme(
      legend.position = c(0.15, 0.90),# Position legend inside the plot (x, y from 0 to 1)
      legend.justification = c("right", "top"),
      legend.background = element_rect(fill = alpha("white", 0.5),color = "black")
    )
}

# Create the plots
plot_c1 <- plot_trendlines(c1_data, "A")
plot_c2 <- plot_trendlines(c2_data, "B")
plot_other <- plot_trendlines(other_data, "C")

combined_plot <- grid.arrange(plot_c1, plot_c2, plot_other, ncol = 3)

# Save the combined plot to a file
ggsave(
  filename = "C0_BLUP_VPD_correlations.png",
  plot = combined_plot,
  width = 18,
  height = 6,
  dpi = 300
)
