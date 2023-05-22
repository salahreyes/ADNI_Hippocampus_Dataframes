library(ggplot2)
library(easystats)

# Rename predictor variables in the data frame
my_data_renamed <- my_data %>%
  rename(MyNewName1 = DxCURREN, MyNewName2 = Dx2, MyNewName3 = Dx3)

# Fit the model
fit <- brm(formula = y ~ MyNewName1 + MyNewName2 + MyNewName3 + (1|group), data = my_data_renamed)

# Calculate the p_direction
result <- p_direction(fit)

plot(result)

# Generate the plot with renamed predictor variables
my_plot <- plot(result)

# Modify the plot
my_plot + 
  ggtitle("My Custom Plot") +
  xlab("") +
  ylab("") +
  theme_bw()

# Save the modified plot to a file
ggsave("my_custom_plot.png", my_plot)

