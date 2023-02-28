# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)

# Variables that will be used for dplyr for dataframe.

#Month_24_Files are in LeCie Drive. 
# Create variable that searches for TSV files in respective directory.
file_list_month24 <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Month_24", 
                                pattern = "*volumes*.tsv", 
                                full.names = TRUE, 
                                recursive = TRUE)
file_list_month24

# Empty data frame
volumes_dt_month24 <- data.frame(NULL)

for(file in file_list_month12){
  # Read the file
  temp_dt_month24 <- data.table::fread(file, na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file, 
                split = "/")[[1]][6]
  # Split file path by -
  b <- strsplit(a, 
                split = "-")
  # Assign ID
  temp_dt_month24$subject <- substring(b[[1]][2], 
                               first = 1, 
                               last=10)
  
  volumes_dt_month24 <- dplyr::bind_rows(volumes_dt_month24, temp_dt_month24)
}
# Modify the dataframe
volumes_dt_month24 <- 
  volumes_dt_month24 %>%
  # Make the dataframe wider
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_")  # forward-pipe operator was missing that is used for chaining commands 
