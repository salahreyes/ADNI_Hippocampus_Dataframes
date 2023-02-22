#load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)


file_list <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Screening", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)
file_list

# Empty data frame
volumes_dt <- data.frame(NULL)

for(file in file_list){
  # Read the file
  temp_dt <- data.table::fread(file, na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file, 
                split = "/")[[1]][6]
  # Split file path by -
  b <- strsplit(a, 
                split = "-")
  # Assign ID
  temp_dt$subject <- substring(b[[1]][2], 
                               first = 1, 
                               last=10)
  
  volumes_dt <- dplyr::bind_rows(volumes_dt, temp_dt)
}

volumes_dt <- volumes_dt %>%
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_")
