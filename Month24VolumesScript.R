# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx, CDR)
gdsmonth24values <- GDSMonth24 # This includes age
diagmonth24 <- DxMonth24 # Has info on dementia diagnosis as well as conversion status from previous timepoint.
freesurfvolsmonth24 <- freesurfermonth24
cdrmonth24values <- CDR_Month24

# Global variables (sex, education, site) that will be used for dplyr for dataframe.
globalvalues <- GlobalInfoPhase1

# Create variable that searches for TSV files in respective directory.
file_list_month24 <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Month_24", 
                                  pattern = "*volumes*.tsv", 
                                  full.names = TRUE, 
                                  recursive = TRUE)

# List of Volumes in TSV files found in above directory
file_list_month24

# Empty data frame
volumes_dt_month24 <- data.frame(NULL)

# Loop to bind TSV files to dataframe
for(file in 1:length(file_list_month24)){
  # Read the file
  temp_dt_month24 <- data.table::fread(file_list_month24[file], na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file_list_month24[file], 
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
  # Make the dataframe wider via hemispheres on one row
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_", values_fn = list) %>%  
  
  # Add etiv data (currently an example of first 5 subjects, have to move freesurfer outputs to Mac.)
  # I do have Ubuntu installed on Windows to install freesurfer 7.2, but encountering errors.
  dplyr::left_join(x =.,
                   y = freesurfvolsmonth24[, c(2, 66, 67)],
                   by = "subject") %>% 
  
  
  # Add Age and GDS values
  dplyr::left_join(x = ., 
                   y = gdsmonth24values[, c(1,3,4)], 
                   by = "subject") %>%
  
  # Add Diagnosis values, not including conversion since ommitting 6 month time points.
  dplyr::left_join(x =.,
                   y = diagmonth24[, c(4, 12)],
                   by = "subject") %>% 
  
  # Add CDR values
  dplyr::left_join(x =.,
                   y = cdrmonth24values[, c(1, 4)],
                   by = "subject") %>% 
  
  
  # Add global values (sex, education and scanner site)
  dplyr::left_join(x =.,
                   y = globalvalues[, c(1, 2, 3, 4)],
                   by = "subject") 


#Change columns that were characters to factors so that residual function can work 
volumes_dt_month24$Sex <- as.numeric(as.factor(volumes_dt_month24$Sex))
volumes_dt_month24$Scanner_Site <- as.numeric(as.factor(volumes_dt_month24$Scanner_Site))

# Here I am trying to make the forumula work with the new dataframe that is numeric.

volumes_numericalmonth24 <- data.frame(NULL)

#sum(is.na(volumes_dt_month24))

#sum(is.na(volumes_numericalmonth24))

#sum(duplicated(volumes_dt_month24))

#sum(duplicated(volumes_numericalmonth24))

#volumes_numericalmonth24 <- volumes_dt_month24[, 2:17]
#volumes_numericalmonth24[] <- lapply(volumes_numericalmonth24, function(x) gsub("c\\(", "", x))
#volumes_numericalmonth24 <- apply(volumes_numericalmonth24, 2, function(x) as.numeric(unlist(x)))



volumes_numericalmonth24 <- volumes_dt_month24[, 2:17]
volumes_numericalmonth24 <- apply(volumes_numericalmonth24, 2, function(x) as.numeric(unlist(x)))
volumes_numericalmonth24 <- as.data.frame(volumes_numericalmonth24)

# Check for duplicated rows
duplicated_rows <- duplicated(volumes_numericalmonth24)

# Print the duplicated rows
print(volumes_numericalmonth24[duplicated_rows, ])

# Remove duplicates
volumes_numericalmonth24 <- unique(volumes_numericalmonth24)
volumes_numericalmonth24 <- cbind(volumes_numericalmonth24,
                           subject = volumes_dt_month24$subject,
                           EstimatedTotalIntraCranialVol = volumes_dt_month24$EstimatedTotalIntraCranialVol,
                           eWBV = volumes_dt_month24$eWBV,
                           GDS_Total_Score = volumes_dt_month24$`GDSCALE Total Score`,
                           DXCURREN = volumes_dt_month24$DXCURREN,
                           Global_CDR = volumes_dt_month24$`Global CDR`,
                           Age = volumes_dt_month24$Age,
                           Sex = volumes_dt_month24$Sex,
                           Education = volumes_dt_month24$Education,
                           Scanner_Site = volumes_dt_month24$Scanner_Site)

#Move subject column to the very left
volumes_numericalmonth24 <- 
  volumes_numericalmonth24 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Make sure the same subjects match my study, so save dataframe to csv
#write.csv(volumes_numericalmonth24, "volumes_numericalmonth24checksubjects.csv", row.names = FALSE)

volumes_numerical <- as.data.frame(volumes_numerical)

vol_resid_func <- function(y) {
  lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = volumes_numericalmonth24)$resid
}

vol_residmonth24 <- as.data.frame(lapply(volumes_numericalmonth24[2:17], vol_resid_func))


#Add the subject column
vol_residmonth24$subject <- volumes_numericalmonth24$subject 

#Move subject column to the very left
vol_residmonth24 <- 
  vol_residmonth24 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Add timepoint values to both dataframes

volumes_numericalmonth24$timepoint <- 3

volumes_numericalmonth24 <-
  volumes_numericalmonth24 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

vol_residmonth24$timepoint <- 3 

vol_residmonth24 <-
  vol_residmonth24 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

# Save vol_resid as vol_resid.csv
write.csv(vol_residmonth24, file = "vol_residmonth24.csv", row.names = FALSE)

# Save volumes_numerical as volumes_numerical.csv
write.csv(volumes_numericalmonth24, file = "volumes_numericalmonth24.csv", row.names = FALSE)

# Get row indices with missing values in a given column
col_name <- "some_column"
na_rows <- which(is.na(volumes_numericalmonth24[[col_name]]))

# Print number of rows with missing values
cat(sprintf("Number of rows with missing %s values: %d\n", col_name, length(na_rows)))

# Print rows with missing values
cat("Rows with missing values:\n")
print(volumes_numericalmonth24[na_rows,])




