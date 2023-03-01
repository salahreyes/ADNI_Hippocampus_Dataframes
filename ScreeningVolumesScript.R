# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx)
gdsscreeningvalues <- GDSScreening # This includes age
diagscreening <- DxScreening # Has info on dementia diagnosis as well as conversion status from previous timepoint.

# Global variables (sex, education, site) that will be used for dplyr for dataframe.
globalvalues <- GlobalInfoPhase1

# Extract 

# Create variable that searches for TSV files in respective directory.
file_list_screening <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Screening", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)

# List of Volumes in TSV files found in above directory
file_list_screening

# Empty data frame
volumes_dt_screening <- data.frame(NULL)

for(file in file_list_screening){
  # Read the file
  temp_dt_screening <- data.table::fread(file, na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file, 
                split = "/")[[1]][6]
  # Split file path by -
  b <- strsplit(a, 
                split = "-")
  # Assign ID
  temp_dt_screening$subject <- substring(b[[1]][2], 
                               first = 1, 
                               last=10)
  
  volumes_dt_screening <- dplyr::bind_rows(volumes_dt_screening, temp_dt_screening)
}
# Modify the dataframe
volumes_dt_screening <- 
  volumes_dt_screening %>%
  # Make the dataframe wider via hemispheres on one row
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_", values_fn = list) %>%  
  
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
  

        
    
