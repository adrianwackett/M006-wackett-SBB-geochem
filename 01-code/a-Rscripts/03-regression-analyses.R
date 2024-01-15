#### LOESS smoother regression model for Supplemental Figure 1 -- Sr/Y ratios across the SBB ####

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

## The two plots above were combined in Adobe Illustrator 2023 due to the incompatibility of plotting the LOESS smoother
## in continuous space while simultaneously generating a boxplot for SBB intrusive suites at discrete 'distance' bins.

#### Linear regression model for Fig 12a -- epsilon Hf vs. U/Pb age of Crawfish Inlet samples ####
# Set up and run linear model
crawfish_hf_lm_model <- lm(e_Hf_t ~ age_Ma, data = crawfish_hf)

# Returna summary of the linear model
summary(crawfish_hf_lm_model)

# Plot of epsilon Hf vs. U/Pb age for Crawfish Inlet samples and one Krestof Island sample
plot12a <- ggplot(data = crawfish_hf, aes(x = age_Ma, y = e_Hf_t, color = pluton, shape = pluton)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
  geom_errorbar(aes(ymin = e_Hf_t - e_Hf_t_error, ymax = e_Hf_t + e_Hf_t_error),
                color = "gray", width = 0.25) +
  geom_point(size = 4, fill = "black", stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add thin black line at y = 0 for CHUR
  scale_shape_manual(values = c("Krestof Island" = 15, "Crawfish Inlet" = 17)) +
  scale_color_manual(values = c("Krestof Island" = "pink", "Crawfish Inlet" = "red")) +
  scale_y_continuous(breaks = seq(-4, 20, by = 2)) +
  xlim(55, 45) +
  ylim(-4, 20) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Hf(t)") +
  annotate("text", x = 54, y = 1, label = paste("R2 =", round(summary(crawfish_hf_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 54, y = 0.4, label = paste("P =", formatC(summary(crawfish_hf_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 45, y = 0.4, label = "CHUR", hjust = 1, size = 5, color = "black") + 
  custom_theme + theme(legend.position = "none")

# Visualize plot12a, which will be panel a in Fig 12 (epsilon Hf vs. U/Pb age)
plot12a

#### Linear regression model for Fig 12b -- epsilon Nd vs. U/Pb age of Crawfish Inlet samples ####
# Set up and run linear model
crawfish_nd_lm_model <- lm(e_Nd_t ~ age_Ma, data = crawfish_nd_sr)

# Return summary of the linear model
summary(crawfish_nd_lm_model)

# Plot of epsilon Nd vs. U/Pb age for Crawfish Inlet samples and one Krestof Island sample
plot12b <- ggplot(data = crawfish_nd_sr, aes(x = age_Ma, y = e_Nd_t, color = pluton, shape = pluton)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
  geom_point(size = 4, fill = "black", stroke = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +  # Add thin black line at y = 0 for CHUR
  scale_shape_manual(values = c("Krestof Island" = 15, "Crawfish Inlet" = 17)) +
  scale_color_manual(values = c("Krestof Island" = "pink", "Crawfish Inlet" = "red")) +
  xlim(55, 45) +
  ylim(-1, 5) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Nd(t)") +
  annotate("text", x = 54, y = 0.25, label = paste("R2 =", round(summary(crawfish_nd_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 54, y = 0.1, label = paste("P =", formatC(summary(crawfish_nd_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 45, y = 0.1, label = "CHUR", hjust = 1, size = 5, color = "black") + 
  custom_theme + theme(legend.position = "none")

# visualize plot12b, which will be panel b in Fig 12 (epsilon Nd vs. U/Pb age)
plot12b

#### Linear regression model for Fig 12c -- 87Sr/86Sr_initial vs. U/Pb age of Crawfish Inlet samples ####
# Set up and run linear model
crawfish_sr_lm_model <- lm(Sr_initial ~ age_Ma, data = crawfish_nd_sr)

# Return summary of the linear model
summary(crawfish_sr_lm_model)

# Plot of 87Sr/86Sr_initial vs. U/Pb age for Crawfish Inlet samples and one Krestof Island sample
plot12c <- ggplot(data = crawfish_nd_sr, aes(x = age_Ma, y = Sr_initial, color = pluton, shape = pluton)) +
  geom_hline(yintercept = 0.7045, linetype = "dashed", color = "black", size = 0.5) +  # Add thin black line at y = 0 for BSE
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
  geom_point(size = 4, fill = "black", stroke = 2) +
  scale_shape_manual(values = c("Krestof Island" = 15, "Crawfish Inlet" = 17)) +
  scale_color_manual(values = c("Krestof Island" = "pink", "Crawfish Inlet" = "red")) +
  xlim(55, 45) +
  ylim(0.7035, 0.7055) +
  labs(x = "U/Pb age (Ma)", y = "87Sr/86Sr_initial") +
  annotate("text", x = 54, y = 0.70376, label = paste("R2 =", round(summary(crawfish_sr_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 54, y = 0.703705, label = paste("P =", formatC(summary(crawfish_sr_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 54, y = 0.70453, label = "BSE", hjust = 1, size = 5, color = "black") + 
  custom_theme + theme(legend.position = "none")

# Visualize plot12c, which will be panel c in Fig 12 (87Sr/86Sr_initial vs. U/Pb age)
plot12c

#### Linear regression model for Fig 12d -- chondrite-normalized Y vs. U/Pb age of Crawfish Inlet samples ####
# Set up and run linear model
crawfish_y_lm_model <- lm(Y_cn ~ age_Ma, data = crawfish_y)

# Return summary of the linear model
summary(crawfish_y_lm_model)

# Plot of chondrite-normalized Y vs. U/Pb age for Crawfish Inlet samples and one Krestof Island sample
plot12d <- ggplot(data = crawfish_y, aes(x = age_Ma, y = Y_cn, color = pluton, shape = pluton)) +
  geom_smooth(method = "lm", se = TRUE, color = "black", fill = "gray", alpha = 0.2) +
  geom_point(size = 4, fill = "black", stroke = 2) +
  scale_shape_manual(values = c("Krestof Island" = 15, "Crawfish Inlet" = 17)) +
  scale_color_manual(values = c("Krestof Island" = "pink", "Crawfish Inlet" = "red")) +
  scale_y_continuous(breaks = seq(0, 24, by = 2)) +
  xlim(55, 45) +
  ylim(0, 25) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Hf(t)") +
  annotate("text", x = 54, y = 4.2, label = paste("R2 =", round(summary(crawfish_y_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 54, y = 3.5, label = paste("P =", formatC(summary(crawfish_y_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  custom_theme + theme(legend.position = "none")

# Visualize plot12d, which will be panel d in Fig 12 (chondrite-normalized Y vs. U/Pb age)
plot12d

#### Combining panels 12a - 12d to generate final Fig 12 ####
# Install and load required packages (gridExtra) if needed
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
library(gridExtra)

# Create a list of plots from saved plots (i.e., plot12a, plot12b, etc)
plot12_list <- list(plot12a, plot12b, plot12c, plot12d)

# Generate four panel plot for Fig. 12
grid.arrange(grobs = plot12_list, ncol(2))

## The four panels from Fig 12 were re-combined in Adobe Illustrator 2023 and final aesthetic modifications were made to the figure shown in the manuscript

#### Linear mixed effects model for Fig 13a -- SBB plutons U/Pb ages (ALL AGES!) versus distance from Sanak Island ####
#### NOTE THAT LMM FOR 13A INCLUDES ALL AVAILABLE AGE DATES #####

# Initial visualization of the data
sbb1 <- ggplot(sbb_age_distance_all, aes(x=distance, y=age_Ma)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray", alpha = 0.3, size = 0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), color = "gray", width = 0.25) +
  geom_point(aes(colour = as.factor(pluton))) + 
  custom_theme + theme(legend.position = "none")

# View initial plot
print(sbb1)

# Checking for normality (validating linear model assumptions)
hist(age_Ma)
shapiro.test(age_Ma)

hist(distance)
shapiro.test(distance)

# Comparing different fixed effects structures using nlme and maximum likelihood (ML) estimation
sbb_lmm_1a<-lme(log10(age_Ma) ~  distance + I((distance)^2) + I((distance)^3), random = ~1|method  ,method="ML",data=d)  
summary(model1a)
model1b<-lme(log10(age_Ma) ~  distance, random = ~1|method  ,method="ML",data=d) 
summary(model1b)
model1c<-lme(log10(age_Ma) ~  distance + I((distance)^2), random = ~1|method  ,method="ML",data=d)
summary(model1c)
