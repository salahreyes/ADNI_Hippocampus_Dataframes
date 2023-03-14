# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx, CDR)
gdsmonth12values <- GDSMonth12 # This includes age
diagmonth12 <- DxMonth12 # Has info on dementia diagnosis as well as conversion status from previous timepoint.
freesurfvolsmonth12 <- freesurfermonth12
cdrmonth12values <- CDR_Month12

# Global variables (sex, education, site) that will be used for dplyr for dataframe.
globalvalues <- GlobalInfoPhase1

# Create variable that searches for TSV files in respective directory.
file_list_month12 <- list.files(path = "D:/HippUnfold_Outputs/Month_12", 
                                pattern = "*volumes*.tsv", 
                                full.names = TRUE, 
                                recursive = TRUE)

# List of Volumes in TSV files found in above directory
file_list_month12

# Empty data frame
volumes_dt_month12 <- data.frame(NULL)

# Loop to bind TSV files to dataframe
for(file in 1:length(file_list_month12)){
  # Read the file
  temp_dt_month12 <- data.table::fread(file_list_month12[file], na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file_list_month12[file], 
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
  # Make the dataframe wider via hemispheres on one row
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_", values_fn = list) %>%  
  
  # Add etiv data (currently an example of first 5 subjects, have to move freesurfer outputs to Mac.)
  # I do have Ubuntu installed on Windows to install freesurfer 7.2, but encountering errors.
  dplyr::left_join(x =.,
                   y = freesurfvolsmonth12[, c(2, 66, 67)],
                   by = "subject") %>% 
  
  
  # Add Age and GDS values
  dplyr::left_join(x = ., 
                   y = gdsmonth12values[, c(1,3,4)], 
                   by = "subject") %>%
  
  # Add Diagnosis values, not including conversion since ommitting 6 month time points.
  dplyr::left_join(x =.,
                   y = diagmonth12[, c(4, 12)],
                   by = "subject") %>% 
  
  # Add CDR values
  dplyr::left_join(x =.,
                   y = cdrmonth12values[, c(1, 4)],
                   by = "subject") %>% 
  
  
  # Add global values (sex, education and scanner site)
  dplyr::left_join(x =.,
                   y = globalvalues[, c(1, 2, 3, 4)],
                   by = "subject") 


#Change columns that were characters to factors so that residual function can work 
volumes_dt_month12$Sex <- as.numeric(as.factor(volumes_dt_month12$Sex))
volumes_dt_month12$Scanner_Site <- as.numeric(as.factor(volumes_dt_month12$Scanner_Site))

# Here I am trying to make the forumula work with the new dataframe that is numeric.

volumes_numericalmonth12 <- data.frame(NULL)

#sum(is.na(volumes_dt_month12))

#sum(is.na(volumes_numericalmonth12))

#sum(duplicated(volumes_dt_month12))

#sum(duplicated(volumes_numericalmonth12))

#volumes_numericalmonth12 <- volumes_dt_month12[, 2:17]
#volumes_numericalmonth12[] <- lapply(volumes_numericalmonth12, function(x) gsub("c\\(", "", x))
#volumes_numericalmonth12 <- apply(volumes_numericalmonth12, 2, function(x) as.numeric(unlist(x)))

volumes_numericalmonth12 <- volumes_dt_month12[, 2:17]
volumes_numericalmonth12 <- apply(volumes_numericalmonth12, 2, function(x) as.numeric(unlist(x)))
volumes_numericalmonth12 <- as.data.frame(volumes_numericalmonth12)

# Check for duplicated rows
duplicated_rows <- duplicated(volumes_numericalmonth12)

# Print the duplicated rows
print(volumes_numericalmonth12[duplicated_rows, ])

# Remove duplicates
volumes_numericalmonth12 <- unique(volumes_numericalmonth12)
volumes_numericalmonth12 <- cbind(volumes_numericalmonth12,
                                  subject = volumes_dt_month12$subject,
                                  EstimatedTotalIntraCranialVol = volumes_dt_month12$EstimatedTotalIntraCranialVol,
                                  eWBV = volumes_dt_month12$eWBV,
                                  GDS_Total_Score= volumes_dt_month12$`GDSCALE Total Score`,
                                  DxCURREN = volumes_dt_month12$DXCURREN,
                                  Global_CDR = volumes_dt_month12$`Global CDR`,
                                  Age = volumes_dt_month12$Age,
                                  Sex = volumes_dt_month12$Sex,
                                  Education = volumes_dt_month12$Education,
                                  Scanner_Site = volumes_dt_month12$Scanner_Site)

#Move subject column to the very left
volumes_numericalmonth12 <- 
  volumes_numericalmonth12 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Make sure the same subjects match my study, so save dataframe to csv
write.csv(volumes_numericalmonth12, "volumes_numericalmonth12checksubjects.csv", row.names = FALSE)


vol_resid_func <- function(y) {
  lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = volumes_numericalmonth12)$resid
}

vol_residmonth12 <- as.data.frame(lapply(volumes_numericalmonth12[2:17], vol_resid_func))

# Create a copy of the dataframe
volumes_clean <- volumes_numericalmonth12

# Subset the dataframe to only include rows with non-missing values in EstimatedTotalIntraCranialVol and eWBV
volumes_clean <- volumes_clean[complete.cases(volumes_clean$EstimatedTotalIntraCranialVol, volumes_clean$eWBV), ]

#Add timepoint values to both dataframes

volumes_numericalmonth12$timepoint <- 2

volumes_numericalmonth12 <-
  volumes_numericalmonth12 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

vol_residmonth12$timepoint <- 2 

vol_residmonth12 <-
  vol_residmonth12 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)



#Add the subject column
vol_residmonth12$subject <- volumes_numericalmonth12$subject 

#Move subject column to the very left
vol_residmonth12 <- 
  vol_residmonth12 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

# Save vol_resid as vol_resid.csv
write.csv(vol_residmonth12, file = "vol_residmonth12.csv", row.names = FALSE)

# Save volumes_numerical as volumes_numerical.csv
write.csv(volumes_numericalmonth12, file = "volumes_numericalmonth12.csv", row.names = FALSE)

# Subset subjects with missing EstimatedIntraCranialVol values
na_subjects <- volumes_dt_month12$subject[is.na(volumes_dt_month12$EstimatedTotalIntraCranialVol)]

# Print number of subjects with missing values
cat(sprintf("Number of subjects with missing EstimatedIntraCranialVol values: %d\n", length(na_subjects)))

# Print list of subjects with missing values
cat("List of subjects with missing EstimatedIntraCranialVol values:\n")
cat(na_subjects, sep = "\n")




