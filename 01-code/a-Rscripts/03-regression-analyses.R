#### Linear regression model for supplemental fig 1 -- Sr/Y ratios across the SBB ####

# Ensure 'distance' and 'Sr_Y' columns are numeric
sbb_SrY_distance_54Si$distance <- as.numeric(sbb_SrY_distance_54Si$distance)
sbb_SrY_distance_54Si$Sr_Y <- as.numeric(sbb_SrY_distance_54Si$Sr_Y)

# Calculate LOESS smoother separately
loess_smooth <- loess(Sr_Y ~ distance, data = sbb_SrY_distance_54Si)

# Plot the correct LOESS smoother
ggplot(sbb_SrY_distance_54Si, aes(x = distance, y = Sr_Y)) +
  geom_boxplot(width = 0.0) +
  stat_smooth(geom = "smooth", method = "loess", formula = y ~ x, se = TRUE, color = "black", aes(group = 1), z = -2) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(0, 2200, by = 200), minor_breaks = seq(0, 2200, by = 100)) +
  labs(x = "Distance from Sanak Island (km)", y = "Sr/Y") +
  custom_theme

# Plot box-and-whiskers at each discrete 'distance' value
ggplot(sbb_SrY_distance_54Si, aes(x = as.factor(distance), y = Sr_Y)) +
  geom_boxplot(width = 0.5) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_y_log10() +
  labs(x = "Distance from Sanak Island (km)", y = "Sr/Y") +
  custom_theme

# Define box colors based on 'distance'
sbb_SrY_distance_54Si$legend <- ifelse(sbb_SrY_distance_54Si$distance == 3, "purple",
  ifelse(sbb_SrY_distance_54Si$distance == 203, "blue",
  ifelse(sbb_SrY_distance_54Si$distance == 1290, "green",
  ifelse(sbb_SrY_distance_54Si$distance == 1360, "yellow",
  ifelse(sbb_SrY_distance_54Si$distance %in% c(1685, 1705), "orange",
  ifelse(sbb_SrY_distance_54Si$distance == 1770, "gold",
  ifelse(sbb_SrY_distance_54Si$distance == 2085, "pink",
  ifelse(sbb_SrY_distance_54Si$distance == 2150, "red", "white"))))))))


# Plot box-and-whiskers with customized colors and legend labels
ggplot(sbb_SrY_distance_54Si, aes(x = as.factor(distance), y = Sr_Y, fill = legend)) +
  geom_boxplot(width = 0.5) +
  scale_fill_manual(values = c(purple = "purple", blue = "blue", green = "green", yellow = "yellow",
  orange = "orange", gold = "gold", pink = "pink", red = "red", white = "white"
  ), labels = c( purple = "Sanak Island", blue = "Nagai Island", green = "Sheep Bay", yellow = "McKinley Peak",
  orange = "Mt. Draper/Stamy", gold = "Novatak Glacier", pink = "Krestof Island", red = "Crawfish", white = "Other studies"
  )) +
  scale_y_log10() +
  labs(x = "Distance from Sanak Island (km)", y = "Sr/Y") +
  theme(legend.position = "bottom") +  # Move legend to the bottom
  custom_theme

## The two plots above were combined in adobe illustrator due to the incompatibility of plotting the LOESS smoother
## in continuous space while simultaneously creating the boxplot at discrete 'distance' bins.

#### Linear regression model for fig 12a -- epsilon Hf vs. U/Pb age in Crawfish Inlet samples ####
# set up and run linear model
crawfish_hf_lm_model <- lm(e_Hf_t ~ age_Ma, data = crawfish_hf)

# return summary of the linear model
summary(crawfish_hf_lm_model)

ggplot(data = crawfish_hf, aes(x = age_Ma, y = e_Hf_t, color = pluton, shape = pluton)) +
  geom_point(size = 4, fill = "black", stroke = 2) +
  geom_errorbar(aes(ymin = e_Hf_t - e_Hf_t_error, ymax = e_Hf_t + e_Hf_t_error),
                color = "gray", width = 0.25) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add thin black line at y = 0
  xlim(55, 45) +
  ylim(-4, 20) +
  scale_shape_manual(values = c("Krestof Island" = 15, "Crawfish Inlet" = 17)) +
  scale_color_manual(values = c("Krestof Island" = "pink", "Crawfish Inlet" = "red")) +
  scale_y_continuous(breaks = seq(-4, 20, by = 2)) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Hf(t)") +
  theme(legend.position = "none") + 
  annotate("text", x = 54, y = 1.5, label = paste("R2 =", round(summary(crawfish_hf_lm_model)$r.squared, 2)), hjust = 1, size = 3) +
  annotate("text", x = 54, y = 0.5, label = paste("P =", formatC(summary(crawfish_hf_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 3) +
  annotate("text", x = 45, y = 0.3, label = "CHUR", hjust = 1, size = 3, color = "black") + 
  custom_theme
  
