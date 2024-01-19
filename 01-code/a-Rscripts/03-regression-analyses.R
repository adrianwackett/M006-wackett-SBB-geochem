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
  xlim(54, 46) +
  ylim(-4, 20) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Hf(t)") +
  annotate("text", x = 53, y = 1, label = paste("R2 =", round(summary(crawfish_hf_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 53, y = 0.4, label = paste("P =", formatC(summary(crawfish_hf_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 46, y = 0.4, label = "CHUR", hjust = 1, size = 5, color = "black") + 
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
  xlim(54, 46) +
  ylim(-1, 5) +
  labs(x = "U/Pb age (Ma)", y = "epsilon Nd(t)") +
  annotate("text", x = 53, y = 0.25, label = paste("R2 =", round(summary(crawfish_nd_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 53, y = 0.1, label = paste("P =", formatC(summary(crawfish_nd_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 46, y = 0.1, label = "CHUR", hjust = 1, size = 5, color = "black") + 
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
  xlim(54, 46) +
  ylim(0.7035, 0.7055) +
  labs(x = "U/Pb age (Ma)", y = "87Sr/86Sr_initial") +
  annotate("text", x = 53, y = 0.70376, label = paste("R2 =", round(summary(crawfish_sr_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 53, y = 0.703705, label = paste("P =", formatC(summary(crawfish_sr_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
  annotate("text", x = 53, y = 0.70453, label = "BSE", hjust = 1, size = 5, color = "black") + 
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
  xlim(54, 46) +
  ylim(0, 25) +
  labs(x = "U/Pb age (Ma)", y = "Y_cn") +
  annotate("text", x = 53, y = 4.2, label = paste("R2 =", round(summary(crawfish_y_lm_model)$r.squared, 2)), hjust = 1, size = 5) +
  annotate("text", x = 53, y = 3.5, label = paste("P =", formatC(summary(crawfish_y_lm_model)$coefficients[2, 4], digits = 2)), hjust = 1, size = 5) +
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

#### Linear mixed effects model for Fig 13a -- SBB pluton ages (ALL AGES!) versus distance from Sanak Island ####
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
hist(sbb_age_distance_all$age_Ma) # normal distribution
shapiro.test(sbb_age_distance_all$age_Ma)

hist(sbb_age_distance_all$distance)
shapiro.test(sbb_age_distance_all$distance)

# Comparing different fixed effects structures using nlme and maximum likelihood (ML) estimation
sbb_lmm_1a <- lme(log10(age_Ma) ~ distance + I((distance)^2) + I((distance)^3), random = ~1|pluton, method="ML", data=sbb_age_distance_all)  
summary(sbb_lmm_1a)

sbb_lmm_1b <- lme(log10(age_Ma) ~ distance, random = ~1|pluton, method="ML", data=sbb_age_distance_all) 
summary(sbb_lmm_1b)

sbb_lmm_1c <- lme(log10(age_Ma) ~ distance + I((distance)^2), random = ~1|pluton, method="ML", data=sbb_age_distance_all)
summary(sbb_lmm_1c)

# Model selection based on lowest AIC score
anova(sbb_lmm_1a, sbb_lmm_1b) 
anova(sbb_lmm_1b, sbb_lmm_1c) # model 1c has the lowest AIC score = best fixed effects structure

# Checking model residuals and QQplots to evaluate model fit for initial nlme model
plot(sbb_lmm_1c)
plot(sbb_lmm_1c,age_Ma~fitted(.))
qqnorm(sbb_lmm_1c,~resid(.)|pluton)

# Comparing different random effects structures using maximum likelihood (ML) estimation
# Random intercept models
SBB_LMM_1A <- lmer(log10(age_Ma) ~  distance + I((distance)^2) + (1 | pluton), data = sbb_age_distance_all, REML = FALSE)
summary(SBB_LMM_1A)

# Random slope models
SBB_LMM_1B <- lmer(log10(age_Ma) ~  distance + I((distance)^2) + (0 + distance | pluton) + (0 + I((distance)^2) | pluton), data = sbb_age_distance_all, REML = FALSE)
summary(SBB_LMM_1B)

# Random slopes and intercepts (slope & intercept calculated independently)
SBB_LMM_1C <- lmer(log10(age_Ma) ~  distance + I((distance)^2) + (1 | pluton) + (0 + distance | pluton), data = sbb_age_distance_all, REML = FALSE)
summary(SBB_LMM_1C)

SBB_LMM_1D <- lmer(log10(age_Ma) ~  distance + I((distance)^2) + (1 | pluton) + (0 + I((distance)^2) | pluton), data = sbb_age_distance_all, REML = FALSE)
summary(SBB_LMM_1D)

# Model selection for random factors based on lowest AIC score
anova(SBB_LMM_1A, SBB_LMM_1B, SBB_LMM_1C, SBB_LMM_1D)
anova(SBB_LMM_1A,SBB_LMM_1C) # random intercept model (1A) is the best overall model fit

# Re-run final model using restricted maximum likelihood estimation (REML)
# Selected random intercept model
sbb_age_distance_all_lmer <- lmer(log10(age_Ma) ~ distance + I((distance)^2) + (1 | pluton), data = sbb_age_distance_all, REML = TRUE)
summary(sbb_age_distance_all_lmer)

# Validate model selection/optimization using lmerTest #
# Install and load required packages (lmerTest) if needed
if (!requireNamespace("lmerTest", quietly = TRUE)) {
  install.packages("lmerTest")
}
library(lmerTest)

# Re-run selected random intercept model
sbb_age_distance_all_lmer <- lmer(log10(age_Ma) ~ distance + I((distance)^2) + (1 | pluton), data = sbb_age_distance_all, REML = TRUE)
summary(sbb_age_distance_all_lmer)

# Extract P- and F-values using Satterthwaite's method and anova table
(aov <- anova(sbb_age_distance_all_lmer))

# Extract r2 value from random intercept model
r2_sbb_age_distance_all_lmer <- r2beta(model=sbb_age_distance_all_lmer,partial=TRUE,method='sgv')
r2_sbb_age_distance_all_lmer

# Create predicted response variable for pluton age from the optimized model
sbb_age_distance_all$predicted_age_Ma <- predict(sbb_age_distance_all_lmer, type = "response")

# Plot of pluton age versus distance from Sanak Is. using all available age dates
plot13a <- ggplot(sbb_age_distance_all, aes(x=distance, y=age_Ma)) + 
  stat_smooth(data=sbb_age_distance_all, aes(distance, (10^sbb_age_distance_all$predicted_age_Ma)), method = "lm", formula = y ~ x + I(x^2), fill = "gray", colour = 'black', alpha = 2/10, size=0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), width = 0.25, colour = 'lightgray') +
  geom_point(aes(shape = pluton, colour = pluton), size = 5, stroke = 0.5) + 
  xlim(0, 2200) +
  ylim(45, 65) +
  scale_shape_manual(values = c('Sanak Is.' = 16, 'Nagai Is.' = 18, 'Aialik' = 16, 'Hive Island' = 18, 'Sheep Bay' = 16, 'McKinley Peak' = 16, 'Mt. Stamy' = 16, 'Mt. Draper' = 16, 'Novatak' = 18, 'Krestof Is.' = 15, 'Crawfish' = 17)) +
  scale_colour_manual(values = c('Sanak Is.' = 'purple', 'Nagai Is.' = 'darkblue', 'Aialik' = 'lightblue', 'Hive Island' = 'palegreen', 'Sheep Bay' = 'green', 'McKinley Peak' = 'yellow', 'Mt. Stamy' = 'orange', 'Mt. Draper' = 'orange', 'Krestof Is.' = 'pink', 'Crawfish' = 'red')) +
  custom_theme + theme(legend.position = "none")

# Visualize plot for panel 13a
plot13a  
  
#### Linear mixed effects model for Fig 13b -- SBB pluton U/Pb ages (Concordant U/Pb ages only!) versus distance from Sanak Island ####
#### NOTE THAT LMMs FOR FIG 13B ONLY INCLUDE CONCORDANT U/PB DATES FROM MAGMATIC ZIRCON #####

# Initial visualization of the data
sbb2 <- ggplot(sbb_age_distance_zircons, aes(x=distance, y=age_Ma)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray", alpha = 0.3, size = 0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), color = "gray", width = 0.25) +
  geom_point(aes(colour = as.factor(pluton))) + 
  custom_theme + theme(legend.position = "none")

# View initial plot
print(sbb2)

# Checking for normality (validating linear model assumptions)
hist(sbb_age_distance_zircons$age_Ma) # normal distribution
shapiro.test(sbb_age_distance_zircons$age_Ma)

hist(sbb_age_distance_zircons$distance)
shapiro.test(sbb_age_distance_zircons$distance)

# Comparing different fixed effects structures using nlme and maximum likelihood (ML) estimation
sbb_lmm_2a <- lme(age_Ma ~ distance + I((distance)^2) + I((distance)^3), random = ~1|pluton, method="ML", data=sbb_age_distance_zircons)  
summary(sbb_lmm_2a)

sbb_lmm_2b <- lme(age_Ma ~ distance, random = ~1|pluton, method="ML", data=sbb_age_distance_zircons) 
summary(sbb_lmm_2b)

sbb_lmm_2c <- lme(age_Ma ~ distance + I((distance)^2), random = ~1|pluton, method="ML", data=sbb_age_distance_zircons)
summary(sbb_lmm_2c)

# Model selection based on lowest AIC score
anova(sbb_lmm_2a, sbb_lmm_2b) 
anova(sbb_lmm_2b, sbb_lmm_2c) # model 2b has the lowest AIC score = best fixed effects structure

# Checking model residuals and QQplots to evaluate model fit for initial nlme model
plot(sbb_lmm_2b)
plot(sbb_lmm_2b,age_Ma~fitted(.))
qqnorm(sbb_lmm_2b,~resid(.)|pluton)

# Comparing different random effects structures using maximum likelihood (ML) estimation
# Random intercept models
SBB_LMM_2A <- lmer(age_Ma ~  distance + (1 | pluton), data = sbb_age_distance_zircons, REML = FALSE)
summary(SBB_LMM_2A)

# Random slope models
SBB_LMM_2B <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = sbb_age_distance_zircons, REML = FALSE)
summary(SBB_LMM_2B)

# Random slopes and intercepts (slope & intercept calculated together at the grouping level)
SBB_LMM_2C <- lmer(age_Ma ~  distance + (1 + distance | pluton), data = sbb_age_distance_zircons, REML = FALSE)
summary(SBB_LMM_2C)

# Random slopes and intercepts (slope & intercept calculated independently)
SBB_LMM_2D <- lmer(age_Ma ~  distance + (1 | pluton) + (0 + distance | pluton), data = sbb_age_distance_zircons, REML = FALSE)
summary(SBB_LMM_2D)

# Model selection for random factors based on lowest AIC score
anova(SBB_LMM_2A, SBB_LMM_2B, SBB_LMM_2C, SBB_LMM_2D)
anova(SBB_LMM_2B,SBB_LMM_2D) # random slope model (2B) is the best overall model fit

# Re-run final random slope model using restricted maximum likelihood estimation (REML)
# Selected random slope model
sbb_age_distance_zircons_lmer <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = sbb_age_distance_zircons, REML = TRUE)
summary(sbb_age_distance_zircons_lmer)

# Validate model selection/optimization using lmerTest #
library(lmerTest)

# Re-run selected random intercept model
sbb_age_distance_zircons_lmer <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = sbb_age_distance_zircons, REML = TRUE)
summary(sbb_age_distance_zircons_lmer)

# Extract P- and F-values using Satterthwaite's method and anova table
(aov <- anova(sbb_age_distance_zircons_lmer))

# Use lmerTest step function to validate manual model selection approach
step_result <- step(sbb_age_distance_zircons_lmer)
step_result

# Retrieve final lmm selected using lmerTest model step function
sbb_age_distance_zircons_final <- get_model(step_result)
sbb_age_distance_zircons_final

# Re-summarize final model for U/Pb age (concordant ages) versus distance from Sanak Is.
summary(sbb_age_distance_zircons_final)

# Re-extract P- and F-values from final model using Satterthwaite's method and anova table
(aov <- anova(sbb_age_distance_zircons_final))

# Create predicted response variable for pluton age from the optimized model
sbb_age_distance_zircons$predicted_UPb_age_Ma <- predict(sbb_age_distance_zircons_final, type = "response")

# Plot of U/Pb age versus distance from Sanak Is. for concordant U/Pb ages only
plot13b_full_sbb <- ggplot(sbb_age_distance_zircons, aes(x=distance, y=age_Ma)) + 
  stat_smooth(data=sbb_age_distance_zircons, aes(distance, sbb_age_distance_zircons$predicted_UPb_age_Ma), method = "lm", se = TRUE, formula = y ~ x, fill = "gray", colour = 'black', alpha = 3/10, size=0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), width = 0.25, colour = 'lightgray') +  
  geom_point(aes(shape = pluton, colour = pluton), size = 5, stroke = 0.5) + 
  xlim(0, 2200) +
  ylim(45, 65) +
  scale_shape_manual(values = c('Sanak Is.' = 16, 'Nagai Is.' = 18, 'Aialik' = 16, 'Hive Island' = 18, 'Sheep Bay' = 16, 'McKinley Peak' = 16, 'Mt. Stamy' = 16, 'Mt. Draper' = 16, 'Novatak' = 18, 'Krestof Is.' = 15, 'Crawfish' = 17)) +
  scale_colour_manual(values = c('Sanak Is.' = 'purple', 'Nagai Is.' = 'darkblue', 'Aialik' = 'lightblue', 'Hive Island' = 'palegreen', 'Sheep Bay' = 'green', 'McKinley Peak' = 'yellow', 'Mt. Stamy' = 'orange', 'Mt. Draper' = 'orange', 'Novatak' = 'gold', 'Krestof Is.' = 'pink', 'Crawfish' = 'red')) +
  custom_theme + theme(legend.position = "none")

# Visualize regression plot for Fig. 13b for full belt regression
plot13b_full_sbb  

#### Linear mixed effects model for Fig 13b -- western SBB pluton U/Pb ages (Concordant U/Pb ages only!) versus distance from Sanak Island ####
#### NOTE THAT LMMs FOR 13B ONLY INCLUDE CONCORDANT U/PB DATES FROM MAGMATIC ZIRCON #####

# Initial visualization of the western SBB data
western_sbb1 <- ggplot(western_sbb, aes(x=distance, y=age_Ma)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray", alpha = 0.3, size = 0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), color = "gray", width = 0.25) +
  geom_point(aes(colour = as.factor(pluton))) + 
  custom_theme + theme(legend.position = "none")

# View initial plot
print(western_sbb1)

# Checking for normality (validating linear model assumptions)
hist(western_sbb$age_Ma) # normal distribution
shapiro.test(western_sbb$age_Ma)

hist(western_sbb$distance)
shapiro.test(western_sbb$distance)

# Comparing different fixed effects structures using nlme and maximum likelihood (ML) estimation
western_sbb_lmm_1 <- lme(age_Ma ~ distance + I((distance)^2) + I((distance)^3), random = ~1|pluton, method="ML", data=western_sbb)  
summary(western_sbb_lmm_1)

western_sbb_lmm_2 <- lme(age_Ma ~ distance, random = ~1|pluton, method="ML", data=western_sbb) 
summary(western_sbb_lmm_2)

western_sbb_lmm_3 <- lme(age_Ma ~ distance + I((distance)^2), random = ~1|pluton, method="ML", data=western_sbb)
summary(western_sbb_lmm_3)

# Model selection based on lowest AIC score
anova(western_sbb_lmm_1, western_sbb_lmm_2) 
anova(western_sbb_lmm_2, western_sbb_lmm_3) # model 2 (for western SBB) has the lowest AIC score = best fixed effects structure

# Checking model residuals and QQplots to evaluate model fit for initial nlme model
plot(western_sbb_lmm_2)
plot(western_sbb_lmm_2,age_Ma~fitted(.))
qqnorm(western_sbb_lmm_2,~resid(.)|pluton)

# Comparing different random effects structures using maximum likelihood (ML) estimation
# Random intercept model
WESTERN_SBB_LMM_1 <- lmer(age_Ma ~  distance + (1 | pluton), data = western_sbb, REML = FALSE)
summary(WESTERN_SBB_LMM_1)

# Random slope model
WESTERN_SBB_LMM_2 <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = western_sbb, REML = FALSE)
summary(WESTERN_SBB_LMM_2)

# Random slope and intercept (slope & intercept calculated independently)
WESTERN_SBB_LMM_3 <- lmer(age_Ma ~  distance + (1 | pluton) + (0 + distance | pluton), data = western_sbb, REML = FALSE)
summary(WESTERN_SBB_LMM_3)

# Model selection for random factors based on lowest AIC score
anova(WESTERN_SBB_LMM_1, WESTERN_SBB_LMM_2) 
anova(WESTERN_SBB_LMM_2, WESTERN_SBB_LMM_3) # random slope model 2 is the best overall model fit for western SBB

# Re-run final random slope model using restricted maximum likelihood estimation (REML)
# Selected random slope model
western_sbb_age_distance_lmer <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = western_sbb, REML = TRUE)
summary(western_sbb_age_distance_lmer)

# Extract P- and F-values using Satterthwaite's method and anova table
(aov <- anova(western_sbb_age_distance_lmer))

# Use lmerTest step function to validate manual model selection approach
step_result <- step(western_sbb_age_distance_lmer)
step_result

# Retrieve final lmm selected using lmerTest model step function
western_sbb_age_distance_final <- get_model(step_result)
western_sbb_age_distance_final

# Re-summarize final model for U/Pb age (concordant ages) versus distance from Sanak Is.
summary(western_sbb_age_distance_final)

# Re-extract P- and F-values from final model using Satterthwaite's method and anova table
(aov <- anova(western_sbb_age_distance_final))

# Extract r2 value from final random slope model
r2_western_sbb_age_distance_final <- r2beta(model=western_sbb_age_distance_final,partial=TRUE,method='sgv')
r2_western_sbb_age_distance_final

# Create predicted response variable for pluton age from the optimized model
western_sbb$predicted_UPb_age_Ma <- predict(western_sbb_age_distance_final, type = "response")

# Plot of U/Pb age versus distance from Sanak Is. for western SBB rocks (concordant U/Pb ages only)
plot13b_western_sbb <- ggplot(western_sbb, aes(x=distance, y=age_Ma)) + 
  stat_smooth(data=western_sbb, aes(distance, western_sbb$predicted_UPb_age_Ma), method = "lm", se = TRUE, formula = y ~ x, fill = "gray", colour = 'black', alpha = 3/10, size=0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), width = 0.25, colour = 'lightgray') +  
  geom_point(aes(shape = pluton, colour = pluton), size = 5, stroke = 0.5) + 
  xlim(0, 2200) +
  ylim(45, 65) +
  scale_shape_manual(values = c('Sanak Is.' = 16, 'Nagai Is.' = 18, 'Aialik' = 16, 'Hive Island' = 18)) +
  scale_colour_manual(values = c('Sanak Is.' = 'purple', 'Nagai Is.' = 'darkblue', 'Aialik' = 'lightblue', 'Hive Island' = 'palegreen')) +
  custom_theme + theme(legend.position = "none")

# Visualize regression model and plot for Fig. 13b (western SBB)
plot13b_western_sbb  

#### Linear mixed effects model for Fig 13b -- eastern SBB pluton U/Pb ages (Concordant U/Pb ages only!) versus distance from Sanak Island ####
#### NOTE THAT LMMs FOR 13B ONLY INCLUDE CONCORDANT U/PB DATES FROM MAGMATIC ZIRCON #####

# Initial visualization of the eastern SBB data
eastern_sbb1 <- ggplot(eastern_sbb, aes(x=distance, y=age_Ma)) +
  geom_smooth(method = "loess", se = TRUE, color = "black", fill = "gray", alpha = 0.3, size = 0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), color = "gray", width = 0.25) +
  geom_point(aes(colour = as.factor(pluton))) + 
  custom_theme + theme(legend.position = "none")

# View initial plot
print(eastern_sbb1)

# Checking for normality (validating linear model assumptions)
hist(eastern_sbb$age_Ma) # normal distribution
shapiro.test(eastern_sbb$age_Ma)

hist(eastern_sbb$distance)
shapiro.test(eastern_sbb$distance)

# Comparing different fixed effects structures using nlme and maximum likelihood (ML) estimation
eastern_sbb_lmm_1 <- lme(age_Ma ~ distance + I((distance)^2) + I((distance)^3), random = ~1|pluton, method="ML", data=eastern_sbb)  
summary(eastern_sbb_lmm_1)

eastern_sbb_lmm_2 <- lme(age_Ma ~ distance, random = ~1|pluton, method="ML", data=eastern_sbb) 
summary(eastern_sbb_lmm_2)

eastern_sbb_lmm_3 <- lme(age_Ma ~ distance + I((distance)^2), random = ~1|pluton, method="ML", data=eastern_sbb)
summary(eastern_sbb_lmm_3)

# Model selection based on lowest AIC score
anova(eastern_sbb_lmm_1, eastern_sbb_lmm_2) 
anova(eastern_sbb_lmm_2, eastern_sbb_lmm_3) # model 2 for eastern SBB has the lowest AIC score = best fixed effects structure

# Checking model residuals and QQplots to evaluate model fit for initial nlme model
plot(eastern_sbb_lmm_2)
plot(eastern_sbb_lmm_2,age_Ma~fitted(.))
qqnorm(eastern_sbb_lmm_2,~resid(.)|pluton)

# Comparing different random effects structures using maximum likelihood (ML) estimation
# Random intercept models
EASTERN_SBB_LMM_1 <- lmer(age_Ma ~  distance + (1 | pluton), data = eastern_sbb, REML = FALSE)
summary(EASTERN_SBB_LMM_1)

# Random slope models
EASTERN_SBB_LMM_2 <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = eastern_sbb, REML = FALSE)
summary(EASTERN_SBB_LMM_2)

# Random slopes and intercepts (slope & intercept calculated independently)
EASTERN_SBB_LMM_3 <- lmer(age_Ma ~  distance + (1 | pluton) + (0 + distance | pluton), data = eastern_sbb, REML = FALSE)
summary(EASTERN_SBB_LMM_3)

# Model selection for random factors based on lowest AIC score
anova(EASTERN_SBB_LMM_1, EASTERN_SBB_LMM_2)
anova(EASTERN_SBB_LMM_2, EASTERN_SBB_LMM_3) # random slope model 2 is the best overall model fit for eastern SBB

# Re-run final random slope model using restricted maximum likelihood estimation (REML)
# Validate model selection/optimization using lmerTest
eastern_sbb_age_distance_lmer <- lmer(age_Ma ~  distance + (0 + distance | pluton), data = eastern_sbb, REML = TRUE)
summary(eastern_sbb_age_distance_lmer)

# Extract P- and F-values using Satterthwaite's method and anova table
(aov <- anova(eastern_sbb_age_distance_lmer))

# Use lmerTest step function to validate manual model selection approach
step_result <- step(eastern_sbb_age_distance_lmer)
step_result

# Retrieve final lmm selected using lmerTest model step function
eastern_sbb_age_distance_final <- get_model(step_result)
eastern_sbb_age_distance_final

# Re-summarize final model for U/Pb age (concordant ages) versus distance from Sanak Is.
summary(eastern_sbb_age_distance_final)

# Re-extract P- and F-values from final model using Satterthwaite's method and anova table
(aov <- anova(eastern_sbb_age_distance_final))

# Extract r2 value from final random slope model
r2_eastern_sbb_age_distance_final <- r2beta(model=eastern_sbb_age_distance_final,partial=TRUE,method='sgv')
r2_eastern_sbb_age_distance_final

# Create predicted response variable for pluton age from the optimized model
eastern_sbb$predicted_UPb_age_Ma <- predict(eastern_sbb_age_distance_final, type = "response")

# Plot of U/Pb age versus distance from Sanak Is. for western SBB rocks (concordant U/Pb ages only)
plot13b_eastern_sbb <- ggplot(eastern_sbb, aes(x=distance, y=age_Ma)) + 
  stat_smooth(data=eastern_sbb, aes(distance, eastern_sbb$predicted_UPb_age_Ma), method = "lm", se = TRUE, formula = y ~ x, fill = "gray", colour = 'black', alpha = 3/10, size=0.4) + 
  geom_errorbar(aes(ymin=age_Ma-age_Ma_error, ymax=age_Ma+age_Ma_error), width = 0.25, colour = 'lightgray') +  
  geom_point(aes(shape = pluton, colour = pluton), size = 5, stroke = 0.5) + 
  xlim(0, 2200) +
  ylim(45, 65) +
  scale_shape_manual(values = c('Sheep Bay' = 16, 'McKinley Peak' = 16, 'Mt. Stamy' = 16, 'Mt. Draper' = 16, 'Novatak' = 18, 'Krestof Is.' = 15, 'Crawfish' = 17)) +
  scale_colour_manual(values = c('Sheep Bay' = 'green', 'McKinley Peak' = 'yellow', 'Mt. Stamy' = 'orange', 'Mt. Draper' = 'orange', 'Novatak' = 'gold', 'Krestof Is.' = 'pink', 'Crawfish' = 'red')) +
  custom_theme + theme(legend.position = "none")

# Visualize regression model and plot for Fig. 13b (eastern SBB)
plot13b_eastern_sbb  

## The three regression models and associated plots forming Fig. 13b were combined in Adobe Illustrator 2023 for simplicity 
## The regression model and plot for Fig. 13A was also added to create the two-panel figure observed in the manuscript
