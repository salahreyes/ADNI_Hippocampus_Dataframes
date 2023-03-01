# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)

# Timepoint-contingent variables that will be used for dplyr for dataframe.
gdsmonth12values <- GDSMonth12

#Month_12_Files are in Seagate Drive. 
# Create variable that searches for TSV files in respective directory.
file_list_month12 <- list.files(path = "D:/HippUnfold_Outputs/Month_12", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)
file_list_month12

# Empty data frame
volumes_dt_month12 <- data.frame(NULL)

for(file in file_list_month12){
  # Read the file
  temp_dt_month12 <- data.table::fread(file, na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file, 
                split = "/")[[1]][4]
  # Split file path by -
  b <- strsplit(a, 
                split = "-")
  # Assign ID
  temp_dt_month12$subject <- substring(b[[1]][2], 
                               first = 1, 
                               last=10)
  
  volumes_dt_month12 <- dplyr::bind_rows(volumes_dt_month12, temp_dt_month12)
}
# Modify the dataframe
volumes_dt_month12 <- 
  volumes_dt_month12 %>%
  # Make the dataframe wider
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_", values_fn = list) %>% # forward-pipe operator was missing that is used for chaining commands
  #Add GDS values
  dplyr::left_join(x = ., # Since commands are chained, this will be .
                 y = gdsmonth12values[, c(1, 4)], # Verify if this name is correct cuz in the screenshot I see it's named GDSValues_Phase1. I added the subject column (key) and the target column (GDS).
                 by = "subject") # Removed the forward-pipe operator here as it led to nothing
  