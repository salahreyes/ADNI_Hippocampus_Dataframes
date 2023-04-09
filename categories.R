library(dplyr)
library(easystats)

#NoDepresNoDemen <- combined_residuals %>%
  #group_by(subject) %>% # group by subject
  #filter(all(GDS_Total_Score >= 0 & GDS_Total_Score <= 4) & all(DxCURREN == 1)) %>% # filter by conditions
  #ungroup()

NoDepresNoDemen <- combined_residuals %>%
  group_by(subject) %>% # group by subject
  filter(all(GDS_Total_Score >= 0 & GDS_Total_Score <= 0) & all(DxCURREN == 1)) %>% # filter by conditions
  ungroup()

SubsyndromalDepresNoDemen <- combined_residuals %>%
  group_by(subject) %>% # group by subject
  filter(all(GDS_Total_Score >= 3 & GDS_Total_Score <= 5) & all(DxCURREN == 1)) %>% # filter by conditions
  ungroup()

SubsyndromalDepresNoDemen <- combined_residuals %>%
  group_by(subject) %>% # group by subject
  filter(all(GDS_Total_Score >= 3 & GDS_Total_Score <= 5) & all(DxCURREN >= 1 & DxCURREN <= 3)) %>% # filter by conditions
  ungroup()


remaining_subjects <- combined_residuals %>%
  filter(!(subject %in% NoDepresNoDemen$subject))


DepresandDemen <- combined_residuals %>% 
  group_by(subject) %>%
  filter(all(GDS_Total_Score >= 5 & GDS_Total_Score <= 15) & all(DxCURREN >= 2 & DxCURREN <= 3)) %>% 
  ungroup()

DepresNoDemen <- combined_residuals %>%
  group_by(subject) %>% # group by subject
  filter(all(GDS_Total_Score >= 5 & GDS_Total_Score <= 15) & all(DxCURREN == 1)) %>% # filter by conditions
  ungroup()

NoDepresDemen <- combined_residuals %>%
  group_by(subject) %>% # group by subject
  filter(all(GDS_Total_Score >= 0 & GDS_Total_Score <= 4) & all(DxCURREN >= 2 & DxCURREN <= 3)) %>% # filter by conditions
  ungroup()

fit <- brm(
  formula = Sub_volumes_L + Sub_volumes_R + CA1_volumes_L + CA1_volumes_R + CA2_volumes_L + CA2_volumes_R 
  + CA3_volumes_L + CA3_volumes_R + CA4_volumes_L + CA4_volumes_R + DG_volumes_L + DG_volumes_R 
  + SRLM_volumes_L + SRLM_volumes_R + Cyst_volumes_L + Cyst_volumes_R  ~  DxCURREN + GDS_Total_Score + timepoint + (1 + timepoint|subject), 
  data = remaining_subjects,
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

summary(fit)

results <- p_direction(fit)

plot(results)
