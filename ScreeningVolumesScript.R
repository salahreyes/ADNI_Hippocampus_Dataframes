# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx, CDR)
gdsscreeningvalues <- GDSScreening # This includes age
diagscreening <- DxScreening # Has info on dementia diagnosis as well as conversion status from previous timepoint.
freesurfvols <- freesurferscreening
cdrsscreeningvalues <- CDR_Screening

# Global variables (sex, education, site) that will be used for dplyr for dataframe.
globalvalues <- GlobalInfoPhase1

# Create variable that searches for TSV files in respective directory.
file_list_screening <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Screening", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)

# List of Volumes in TSV files found in above directory
file_list_screening

# Empty data frame
volumes_dt_screening <- data.frame(NULL)

# Loop to bind TSV files to dataframe
for(file in 1:length(file_list_screening)){
  # Read the file
  temp_dt_screening <- data.table::fread(file_list_screening[file], na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file_list_screening[file], 
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
  
  # Add etiv data (currently an example of first 5 subjects, have to move freesurfer outputs to Mac.)
  # I do have Ubuntu installed on Windows to install freesurfer 7.2, but encountering errors.
  dplyr::left_join(x =.,
                   y = freesurfvols[, c(2, 66, 67)],
                   by = "subject") %>% 
  
  
   # Add Age and GDS values
  dplyr::left_join(x = ., 
                   y = gdsscreeningvalues[, c(1,3,4)], 
                   by = "subject") %>%
  
  # Add Diagnosis values, not including conversion since ommitting 6 month time points.
  dplyr::left_join(x =.,
                   y = diagscreening[, c(4, 12)],
                   by = "subject") %>% 
  
  # Add CDR values
  dplyr::left_join(x =.,
                   y = cdrsscreeningvalues[, c(1, 4)],
                   by = "subject") %>% 
  
  
  # Add global values (sex, education and scanner site)
  dplyr::left_join(x =.,
                   y = globalvalues[, c(1, 2, 3, 4)],
                   by = "subject") 


#Change columns that were characters to factors so that residual function can work 
volumes_dt_screening$Sex <- as.numeric(as.factor(volumes_dt_screening$Sex))
volumes_dt_screening$Scanner_Site <- as.numeric(as.factor(volumes_dt_screening$Scanner_Site))

# Here I am trying to make the forumula work with the new dataframe that is numeric.

volumes_numerical <- data.frame(NULL)

volumes_numerical <- volumes_dt_screening[, 2:17]
volumes_numerical <- apply(volumes_numerical, 2, function(x) as.numeric(unlist(x)))
volumes_numerical <- as.data.frame(volumes_numerical)
# Check for duplicated rows
duplicated_rows <- duplicated(volumes_numerical)

# Print the duplicated rows
print(volumes_numerical[duplicated_rows, ])

# Remove duplicates
volumes_numerical <- unique(volumes_numerical)
volumes_numerical <- cbind(volumes_numerical,
                           subject = volumes_dt_screening$subject,
                           EstimatedTotalIntraCranialVol = volumes_dt_screening$EstimatedTotalIntraCranialVol,
                           eWBV = volumes_dt_screening$eWBV,
                           Age = volumes_dt_screening$Age,
                           Sex = volumes_dt_screening$Sex,
                           Education = volumes_dt_screening$Education,
                           Scanner_Site = volumes_dt_screening$Scanner_Site)

#Move subject column to the very left
volumes_numerical <- 
  volumes_numerical %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Add timepoint value, move to very left
volumes_numerical$timepoint <- 1

volumes_numerical <- 
  volumes_numerical %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)


volumes_numerical <- as.data.frame(volumes_numerical)

vol_resid_func <- function(y) {
  lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = volumes_numerical)$resid
}

vol_resid <- as.data.frame(lapply(volumes_numerical[2:17], vol_resid_func))


#Add the subject column
vol_resid$subject <- volumes_numerical$subject 

#Move subject column to the very left
vol_resid <- 
  vol_resid %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Add timepoint 

vol_resid$timepoint <- 1

vol_resid <- 
  vol_resid %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

# Save vol_resid as vol_resid.csv
write.csv(vol_resid, file = "vol_residscreening.csv", row.names = FALSE)

# Save volumes_numerical as volumes_numerical.csv
write.csv(volumes_numerical, file = "volumes_numericalscreening.csv", row.names = FALSE)




