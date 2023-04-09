# Function for Sub_volumes_L
fit_Sub_volumes_L <- brm(
  formula = Sub_volumes_L ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
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

result <- p_direction(fit_Sub_volumes_L)

plot(result)

# Function for Sub_volumes_R
fit_Sub_volumes_R <- brm(
  formula = Sub_volumes_R ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
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

result <- p_direction(fit_Sub_volumes_R)

plot(result)

# Function for CA1_volumes_L
fit_CA1_volumes_L <- brm(
  formula = CA1_volumes_L ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
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

result <- p_direction(fit_CA1_volumes_L)

plot(result)

# Function for CA1_volumes_R
fit_CA1_volumes_R <- brm(
  formula = CA1_volumes_R ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
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

result <- p_direction(fit_CA1_volumes_R)

plot(result)

fit_CA2_volumes_L <- brm(
  formula = CA1_volumes_R ~ DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
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

