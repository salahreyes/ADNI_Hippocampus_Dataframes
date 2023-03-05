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
 
  # Add Age and GDS values
  dplyr::left_join(x = ., 
                   y = gdsscreeningvalues[, c(1,3,4)], 
                   by = "subject") %>%  
  
  # Add diagnosis data
  dplyr::left_join(x =., 
                   y = diagscreening[, c(4, 12, 13)],
                   by = "subject") %>% 
  
  # Add global values (sex, education and scanner site)
  dplyr::left_join(x =.,
                   y = globalvalues[, c(1, 2, 3, 4)],
                   by = "subject") 
  