install.packages("brms")
library(brms)
install.packages("easystats")
library(easystats)

#Group subfields into hemispheres
#left_cols <- grep("_L", names(combined_volumes), value = TRUE)

# Select columns for right hemisphere
#right_cols <- grep("_R", names(combined_volumes), value = TRUE)

# Create hemisphere variable
#combined_volumes$left_hemisphere <- rowSums(combined_volumes[, c(3, 5, 7, 9, 11, 13, 15, 17)])
#combined_volumes$right_hemisphere <- rowSums(combined_volumes[, c(4, 6, 8, 10, 12, 14, 16, 18)])
#combined_volumes$left_hemi <- ifelse(grepl("_L", names(combined_volumes)), 1, 0)

# Fit a mixed effects model
fit <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# Summarize the model results 
summary(fit) 

result<- p_direction(fit)

plot(result)

# create interaction variable
combined_residualsnew$gds_time_dx <- combined_residualsnew$GDS_Total_Score * combined_residualsnew$timepoint * combined_residualsnew$DxCURREN

fit <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + gds_time_dx + (1 + timepoint|subject), 
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

# run brms model with interaction term
model <- brm(CA1_volumes_L ~ GDS_Total_Score + timepoint + DxCURREN + gds_time_dx + (1|subject),
             data = data, family = gaussian())

fit_L <- brm(
  formula = Sub_volumes_L + CA1_volumes_L + CA2_volumes_L + CA3_volumes_L + CA4_volumes_L + DG_volumes_L + SRLM_volumes_L + Cyst_volumes_L ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint | subject),
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

result_L <- p_direction(fit_L)

plot(result_L)

fit_R <- brm(
  formula = Sub_volumes_R + CA1_volumes_R + CA2_volumes_R + CA3_volumes_R + CA4_volumes_R + DG_volumes_R + SRLM_volumes_R + Cyst_volumes_R ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint | subject),
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

result_R <- p_direction(fit_R)

plot(result_R)

# Define a list of response variables
response_vars <- c("Sub_volumes_L", "Sub_volumes_R", "CA1_volumes_L", "CA1_volumes_R", 
                   "CA2_volumes_L", "CA2_volumes_R", "CA3_volumes_L", "CA3_volumes_R",
                   "CA4_volumes_L", "CA4_volumes_R", "DG_volumes_L", "DG_volumes_R",
                   "SRLM_volumes_L", "SRLM_volumes_R", "Cyst_volumes_L", "Cyst_volumes_R")

# Define a function to fit the model for each response variable and save the results and plots
fit_and_plot <- function(var) {
  
  # Fit the model
  fit <- brm(
    formula = as.formula(paste(var, " ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject)")),
    data = combined_residuals,
    family = gaussian(),
    prior = c(
      prior(normal(0, 10), class = "b"),
      prior(student_t(3, 0, 10), class = "sd"),
      prior(student_t(3, 0, 10), class = "sigma")
    ),
    iter = 4000,
    warmup = 1000,
    chains = 4,
    cores = 4
  )
  

  
  
  # Summarize the model results
  summary(fit)
  
  # Compute the directional p-value
  result <- p_direction(fit)
  
  # Plot the results and save the plot
  plot(result)
  ggsave(paste(var, "_p_direction_plot.png", sep = ""), plot = last_plot())
  
  # Save the posterior samples
  posterior <- as_draws(fit)
  write_csv(posterior, paste(var, "_posterior_samples.csv", sep = ""))
  
}

# Loop over the response variables and call the function for each
for (var in response_vars) {
  fit_and_plot(var)
}

fitgds <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R ~  DxCURREN + GDS_Total_Score + timepoint + gds_time_dx + (1 + gds_time_dx|subject), 
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary(fitgds) 

result<- p_direction(fitgds)

plot(result)

fit3 <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R ~ GDS_Total_Score + DxCURREN + 
    GDS_Total_Score:DxCURREN + (1 + timepoint|subject),
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary(fit3)

result3<- p_direction(fit3)

plot(result3)


fit4 <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R ~ GDS_Total_Score + DxCURREN + 
    DxCURREN:GDS_Total_Score + (1 + timepoint|subject),
  data = combined_residualsnew,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(student_t(3, 0, 10), class = "sd"),
    prior(student_t(3, 0, 10), class = "sigma")
  ),
  iter = 4000,
  warmup = 1000,
  chains = 4,
  cores = 4
)

summary(fit4)

result4 <- p_direction(fit4)

plot(result4)

fit5 <- brm(Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + 
              CA2_volumes_R + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + 
              DG_volumes_R + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R ~ GDS_Total_Score + 
              DxCURREN + GDS_Total_Score:DxCURREN + DxCURREN:GDS_Total_Score + (1 + timepoint | subject),
            data = combined_residualsnew, family = gaussian())
summary(fit5)
result <- p_direction(fit5)
plot(result)
