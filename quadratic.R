library(ggplot2)
library(tidyverse)
library(conflicted)

library(effects)

library(ggplot2)

library(dplyr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(ggplot2)

library(dplyr)

library(ggplot2)

# Create scatterplot
library(ggplot2)

# Plotting the averaged hippocampal volumes
ggplot(data = combined_residualsnew, aes(x = factor(timepoint), y = avg_residual, group = subject)) +
  geom_point(alpha = 0.5, size = 2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
  theme_classic() +
  xlab("Timepoint") +
  ylab("Average Residualized Hippocampal Volume") +
  ggtitle("Average Residualized Hippocampal Volume over Time") +
  theme(plot.title = element_text(hjust = 0.5)) 

library(ggplot2)

# Read in data
data <- combined_residualsnew

# Plot subfield volumes by time for each subject, colored by depression
ggplot(data, aes(x = timepoint, y = avg_residual,)) +
  geom_line(aes(group = subject), alpha = .5) +
  scale_color_gradient(low = "blue", high = "red") +
  xlab("Time (years)") +
  ylab("Hippocampal Volumes (residualized)")

library(dplyr)
library(ggplot2)

# Group the data by timepoint and calculate the mean of avg_residual
collapsed_data <- combined_residualsnew %>%
  group_by(timepoint) %>%
  summarize(avg_residual = mean(avg_residual))

# Create a quadratic model using lm()
quad_model <- lm(avg_residual ~ poly(timepoint, 2), data = collapsed_data)

# Create a data frame for the quadratic line using predict()
quad_line <- data.frame(timepoint = seq(min(collapsed_data$timepoint), max(collapsed_data$timepoint), length.out = 100))
quad_line$avg_residual <- predict(quad_model, newdata = quad_line)

# Plot the quadratic line and the collapsed data
ggplot(collapsed_data, aes(x = timepoint, y = avg_residual)) +
  geom_point() +
  geom_line(data = quad_line, aes(x = timepoint, y = avg_residual), color = "blue") +
  xlab("Time (years)") +
  ylab("Avg_residual (collapsed)") +
  ggtitle("Quadratic Visualization of Avg_residual by Time")

library(dplyr)

# calculate the IQR for each timepoint
collapsed_data <- collapsed_data %>%
  group_by(timepoint) %>%
  mutate(q1 = quantile(avg_residual, 0.25),
         q3 = quantile(avg_residual, 0.75),
         iqr = q3 - q1)

# remove the data points outside the range of 1.5*IQR
collapsed_data_outliers_removed <- collapsed_data %>%
  filter(avg_residual >= q1 - 1.5*iqr & avg_residual <= q3 + 1.5*iqr)

# fit the quadratic model with the filtered data
quad_model <- lm(avg_residual ~ poly(timepoint, 2), data = collapsed_data_outliers_removed)

# create the quadratic line with the filtered data
quad_line <- data.frame(timepoint = seq(min(collapsed_data_outliers_removed$timepoint), max(collapsed_data_outliers_removed$timepoint), length.out = 100))
quad_line$avg_residual <- predict(quad_model, newdata = quad_line)

# plot the quadratic line and the collapsed data without outliers
ggplot(collapsed_data_outliers_removed, aes(x = timepoint, y = avg_residual)) +
  geom_point() +
  geom_line(data = quad_line, aes(x = timepoint, y = avg_residual), color = "blue") +
  xlab("Time (years)") +
  ylab("Avg_residual (collapsed)") +
  ggtitle("Quadratic Visualization of Avg_residual by Time (outliers removed)")


library(ggeffects)

ggpredict(fit1, c("DxCURREN", "timepoint"), type = "fe")
install.packages("emmeans")
library(emmeans)

emm <- emmeans(fit1, "DxCURREN", by = "timepoint")
plot(emm)

library(bayestestR)

marginal_effects(fit1, effects = "gds_dx")

conditional_effects(fit1, effects = "gds_dx")

conditional_effects(fit2, effects = "dx_gds")

# Load required packages
library(brms)
library(ggplot2)

# Create a grid of new data points to cover the range of the predictor variables
newdata <- expand.grid(DxCURREN = unique(combined_residualsnew$DxCURREN),
                       GDS_Total_Score = unique(combined_residualsnew$GDS_Total_Score),
                       timepoint = unique(combined_residualsnew$timepoint),
                       gds_dx = unique(combined_residualsnew$gds_dx),
                       dx_gds = unique(combined_residualsnew$dx_gds))

# Use predict() function to obtain the predicted values for each model
pred1 <- predict(fit1, newdata = newdata, allow_new_levels = TRUE)
pred2 <- predict(fit2, newdata = newdata, allow_new_levels = TRUE)

# Combine the predicted values into a single data frame
pred_df <- data.frame(newdata, pred1 = pred1$fit, pred2 = pred2$fit)

# Create a plot with both conditional effects
ggplot(pred_df, aes(x = DxCURREN, y = pred1, color = factor(timepoint))) +
  geom_line(aes(group = interaction(factor(timepoint), gds_dx)), linetype = "dashed") +
  geom_line(aes(y = pred2, color = factor(timepoint), group = interaction(factor(timepoint), dx_gds))) +
  scale_color_discrete(name = "Timepoint") +
  xlab("DxCURREN") + ylab("Predicted Volumes") +
  ggtitle("Combined Conditional Effects Plot")

