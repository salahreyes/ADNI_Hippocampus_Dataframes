# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)
library(purrr)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx, CDR)
gdsmonth48values <- GDSMonth48 # This includes age
diagmonth48 <- DxMonth48 # Has info on dementia diagnosis as well as conversion status from previous timepoint.
freesurfvolsmonth48 <- longfreesurfmonth48
cdrmonth48values <- CDR_Month48

# Global variables (sex, education, site) that will be used for dplyr for dataframe.
globalvalues <- GlobalInfoPhase1

# Create variable that searches for TSV files in respective directory.
file_list_month48 <- list.files(path = "D:/HippUnfold_Outputs/Month_48", 
                                pattern = "*volumes*.tsv", 
                                full.names = TRUE, 
                                recursive = TRUE)

# List of Volumes in TSV files found in above directory
file_list_month48

# Empty data frame
volumes_dt_month48 <- data.frame(NULL)

# Loop to bind TSV files to dataframe
for(file in 1:length(file_list_month48)){
  # Read the file
  temp_dt_month48 <- data.table::fread(file_list_month48[file], na.strings = c("NA", "NaN", "", "?"))
  
  # Change the subject's ID from BIDS' to ADNI's
  
  # Split file path by /
  a <- strsplit(file_list_month48[file], 
                split = "/")[[1]][4]
  # Split file path by -
  b <- strsplit(a, 
                split = "-")
  # Assign ID
  temp_dt_month48$subject <- substring(b[[1]][2], 
                                       first = 1, 
                                       last=10)
  
  volumes_dt_month48 <- dplyr::bind_rows(volumes_dt_month48, temp_dt_month48)
}
# Modify the dataframe
volumes_dt_month48 <- 
  volumes_dt_month48 %>%
  # Make the dataframe wider via hemispheres on one row
  pivot_wider(names_from = hemi, values_from = c("Sub", "CA1", "CA2", "CA3", "CA4", "DG", "SRLM", "Cyst"),
              names_prefix = "volumes_", values_fn = list) %>%  
  
  # Add etiv data (currently an example of first 5 subjects, have to move freesurfer outputs to Mac.)
  # I do have Ubuntu installed on Windows to install freesurfer 7.2, but encountering errors.
  dplyr::left_join(x =.,
                   y = freesurfvolsmonth48[, c(2, 65, 66)],
                   by = "subject") %>% 
  
  
  # Add Age and GDS values
  dplyr::left_join(x = ., 
                   y = gdsmonth48values[, c(1,3,4)], 
                   by = "subject") %>%
  
  # Add Diagnosis values, not including conversion since ommitting 6 month time points.
  dplyr::left_join(x =.,
                   y = diagmonth48[, c(4, 12)],
                   by = "subject") %>% 
  
  # Add CDR values
  dplyr::left_join(x =.,
                   y = cdrmonth48values[, c(1, 4)],
                   by = "subject") %>% 
  
  
  # Add global values (sex, education and scanner site)
  dplyr::left_join(x =.,
                   y = globalvalues[, c(1, 2, 3, 4)],
                   by = "subject") 


#Change columns that were characters to factors so that residual function can work 
volumes_dt_month48$Sex <- as.numeric(as.factor(volumes_dt_month48$Sex))
volumes_dt_month48$Scanner_Site <- as.numeric(as.factor(volumes_dt_month48$Scanner_Site))

# Here I am trying to make the forumula work with the new dataframe that is numeric.

volumes_numericalmonth48 <- data.frame(NULL)

sum(is.na(volumes_dt_month48))

sum(is.na(volumes_numericalmonth48))

sum(duplicated(volumes_dt_month48))

sum(duplicated(volumes_numericalmonth48))

#volumes_numericalmonth48 <- volumes_dt_month48[, 2:17]
#volumes_numericalmonth48[] <- lapply(volumes_numericalmonth48, function(x) gsub("c\\(", "", x))
#volumes_numericalmonth48 <- apply(volumes_numericalmonth48, 2, function(x) as.numeric(unlist(x)))

volumes_numericalmonth48 <--data.frame(NULL)

volumes_numericalmonth48 <- volumes_dt_month48[, 2:17]
volumes_numericalmonth48 <- apply(volumes_numericalmonth48, 2, function(x) as.numeric(unlist(x)))
volumes_numericalmonth48 <- as.data.frame(volumes_numericalmonth48)

# Check for duplicated rows
duplicated_rows <- duplicated(volumes_numericalmonth48)

# Print the duplicated rows
print(volumes_numericalmonth48[duplicated_rows, ])

# Remove duplicates
volumes_numericalmonth48 <- unique(volumes_numericalmonth48)
volumes_numericalmonth48 <- cbind(volumes_numericalmonth48,
                                  subject = volumes_dt_month48$subject,
                                  EstimatedTotalIntraCranialVol = volumes_dt_month48$EstimatedTotalIntraCranialVol,
                                  eWBV = volumes_dt_month48$eWBV,
                                  GDS_Total_Score= volumes_dt_month48$`GDSCALE Total Score`,
                                  DxCURREN = volumes_dt_month48$DXCURREN,
                                  Global_CDR = volumes_dt_month48$`Global CDR`,
                                  Age = volumes_dt_month48$Age,
                                  Sex = volumes_dt_month48$Sex,
                                  Education = volumes_dt_month48$Education,
                                  Scanner_Site = volumes_dt_month48$Scanner_Site)

#Move subject column to the very left
volumes_numericalmonth48 <- 
  volumes_numericalmonth48 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Make sure the same subjects match my study, so save dataframe to csv
#write.csv(volumes_numericalmonth48, "volumes_numericalmonth48checksubjects.csv", row.names = FALSE)


vol_resid_func <- function(y) {
  lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = volumes_numericalmonth48)$resid
}

vol_residmonth48 <- as.data.frame(lapply(volumes_numericalmonth48[2:17], vol_resid_func))


#Add the subject column
vol_residmonth48$subject <- volumes_numericalmonth48$subject 

#Move subject column to the very left
vol_residmonth48 <- 
  vol_residmonth48 %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

#Add timepoint values to both dataframes

volumes_numericalmonth48$timepoint <- 5

volumes_numericalmonth48 <-
  volumes_numericalmonth48 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

vol_residmonth48$timepoint <- 5 

vol_residmonth48 <-
  vol_residmonth48 %>% 
  dplyr::relocate(timepoint, .before = Sub_volumes_L)

# Save vol_resid as vol_resid.csv
write.csv(vol_residmonth48, file = "vol_residmonth48new.csv", row.names = FALSE)

