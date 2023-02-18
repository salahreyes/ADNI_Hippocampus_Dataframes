library(data.table)
library(dplyr)

file_list <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Screening", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)
file_list

volumes_dt <- data.frame(NULL)

for(file in file_list){
  temp_dt <- fread(file, na.strings = c("NA", "NaN", "", "?"))
  volumes_dt <- bind_rows(volumes_dt, temp_dt)
  gc()
}

volumes_dt
volumes_dt <- volumes_dt %>%
  tidyr::pivot_wider(names_from = hemi, values_from = c("Sub", 
                                                        "CA1", 
                                                        "CA2", 
                                                        "CA3", 
                                                        "CA4", 
                                                        "DG", 
                                                        "SRLM", 
                                                        "Cyst")) %>%

volumes_dt  

