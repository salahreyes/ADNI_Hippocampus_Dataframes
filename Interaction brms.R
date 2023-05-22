library(easystats)
library(ggplot2)
install.packages("ggeffects")
library(ggeffects)
install.packages("effects")
library(effects)

# create interaction variables
combined_residualsnew$gds_dx <- combined_residualsnew$GDS_Total_Score * combined_residualsnew$DxCURREN
combined_residualsnew$dx_gds <- combined_residualsnew$DxCURREN * combined_residualsnew$GDS_Total_Score

# fit model with one interaction term
fit1 <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + gds_dx + (1 + timepoint|subject), 
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

# fit model with the other interaction term
fit2 <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + dx_gds + (1 + timepoint|subject), 
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

summary(fit1)

result <- p_direction(fit1)

plot(result)

fit3 <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + dx_gds + (1 + timepoint|subject), 
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

