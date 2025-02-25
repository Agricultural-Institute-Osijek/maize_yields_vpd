# Load libraries
library(lme4)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)
library(ggrepel)

# Load the dataset
cycle1_data <- read.csv("C1_15_19.csv")

# Filter out rows with NA in Above_median_BLUP
cycle1_data <- cycle1_data %>% filter(!is.na(Above_median_BLUP))

# Fit the mixed-effects model
lmod <- lmer(Yield ~ (1|GEN) + (1|ENV) + (1|GEN:ENV) + (1|REP:ENV), data = cycle1_data)

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
env_means <- cycle1_data %>%
  group_by(ENV) %>%
  summarise(VPD_mean = mean(VPD_flow, na.rm = TRUE))

# Merge BLUPs with environmental means
merged_data <- blups_df %>%
  left_join(env_means, by = "ENV")

# Add Above_median_BLUP group information
group_data <- cycle1_data %>% select(GEN, Above_median_BLUP) %>% distinct()
merged_data <- merged_data %>%
  left_join(group_data, by = "GEN")

# Calculate mean values for each group
group_means <- merged_data %>%
  group_by(Above_median_BLUP, ENV) %>%
  summarise(
    Yield_mean = mean(Yield, na.rm = TRUE),
    VPD_mean = mean(VPD_mean, na.rm = TRUE),
    .groups = "drop"
  )

# Define a function to calculate R² and predict values for trendlines
calculate_trendline <- function(data) {
  model <- lm(Yield_mean ~ VPD_mean, data = data)
  r2 <- summary(model)$r.squared
  data <- data %>%
    mutate(
      Predicted_Yield = predict(model, newdata = data.frame(VPD_mean = VPD_mean))
    )
  list(data = data, r2 = round(r2, 2))
}

# Calculate trendlines for each group
above_median <- group_means %>% filter(Above_median_BLUP == "AM")
below_median <- group_means %>% filter(Above_median_BLUP == "BM")

above_trendline <- calculate_trendline(above_median)
below_trendline <- calculate_trendline(below_median)

# Combine the results
trendline_data <- bind_rows(
  above_trendline$data %>% mutate(Group = "Above Median BLUP"),
  below_trendline$data %>% mutate(Group = "Below Median BLUP")
)

# Create the plot and assign it to a variable
p <- ggplot(trendline_data, aes(x = VPD_mean, y = Yield_mean, color = Group)) +
  geom_line(aes(y = Predicted_Yield), linewidth = 1) +  # Use linewidth instead of size
  geom_point(size = 3) +
  labs(
    x = "VPD (kPa)",
    y = "Yield (t/ha)",
    color = "Group"
  ) +
  theme_bw() +
  theme(
    legend.background = element_blank(),
    legend.title = element_text(size = 12, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    axis.title = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_label_repel(
    data = data.frame(
      Group = c("Above Median BLUP", "Below Median BLUP"),
      VPD_mean = c(max(above_median$VPD_mean), max(below_median$VPD_mean)),
      Yield_mean = c(max(above_trendline$data$Predicted_Yield), max(below_trendline$data$Predicted_Yield)),
      Label = c(
        paste0("R² = ", above_trendline$r2),
        paste0("R² = ", below_trendline$r2)
      )
    ),
    aes(x = VPD_mean, y = Yield_mean, label = Label),
    nudge_x = 0.1,
    size = 4,
    color = "black",
    fill = "white",
    segment.color = "grey50",
    show.legend = FALSE
  ) +
  guides(color = guide_legend(override.aes = list(fill = NA))) +
  theme(
    legend.position = c(0.25, 0.20),  # Position legend inside the plot (x, y from 0 to 1)
    legend.justification = c("right", "top"),
    legend.background = element_rect(fill = alpha("white", 0.5),color = "black"),
    legend.title.align = 0.5
  )

  #ggtitle("BLUP Correlations for Above/Below Median Groups")

# Display the plot
print(p)

# Save the plot using ggsave()
ggsave(
  filename = "C1_Above_Below_Median_BLUP_Correlations.png",
  plot = p,
  width = 8,
  height = 6,
  dpi = 300
)
