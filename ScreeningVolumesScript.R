# Load necessary libraries
library(dplyr)
library(tidyr)
library(data.table)

# Timepoint-contingent variables that will be used for dplyr for dataframe. (GDS, age, Dx, CDR)
gdsscreeningvalues <- GDSScreening # This includes age
diagscreening <- DxScreening # Has info on dementia diagnosis as well as conversion status from previous timepoint.
freesurfvols <- examplefreesurfoutput
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

# Create a subset of the first 5 rows of the data
subset_data <- as.data.frame(volumes_dt_screening[1:5, ])

#Change columns that were characters to factors so that residual function can work 
subset_data$Sex <- as.numeric(as.factor(subset_data$Sex))
subset_data$Scanner_Site <- as.numeric(as.factor(subset_data$Scanner_Site))

#This is the previous residual function, but it was not working as lapply was passing 'y' columns as lists.
#vol_resid_func <- function(y) resid(lm(y ~ subset_data$EstimatedTotalIntraCranialVol + subset_data$eWBV + subset_data$Age + subset_data$Sex + subset_data$Education + subset_data$Scanner_Site, data = subset_data))

#This is the residual function that seems to work.
vol_resid_func <- function(y) {
  y <- unlist(y)
  resid(lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = subset_data))
}


#Apply the function to the subset of data using lapply() 
vol_resid <- as.data.frame(lapply(subset_data[2:17], vol_resid_func))

# Test trying to make residuals from original datframe, leaving out freesurfer data since not ready.
vol_resid_func <- function(y) {
  y <- unlist(y)
  resid(lm(y ~ Age + Sex + Education + Scanner_Site, data = volumes_dt_screening))
}

# Test trying to use the data on the original dataframe
vol_resid <- as.data.frame(lapply(volumes_dt_screening[2:17], vol_resid_func))
vol_resid_func <- function(y) {
  resid(lm(y ~ Age + Sex + Education + Scanner_Site, data = volumes_dt_screening))
}

# Apply the function to each column of the data frame
vol_resid <- as.data.frame(lapply(volumes_dt_screening[2:17], vol_resid_func))

vol_resid_func <- function(y) {
  y <- unlist(y)
  lm_obj <- lm(y ~ Age + Sex + Education + Scanner_Site, data = na.omit(volumes_dt_screening))
  resid(lm_obj)
}

vol_resid <- as.data.frame(lapply(volumes_dt_screening[2:17], vol_resid_func))



vol_resid_func <- function(y, data) {
  y <- unlist(y)
  resid(lm(y ~ EstimatedTotalIntraCranialVol + eWBV + Age + Sex + Education + Scanner_Site, data = data))
}

#Apply the function to the subset of data using lapply() 
vol_resid <- as.data.frame(lapply(subset_data[2:17], vol_resid_func, data = subset_data))


#Add the subject column
vol_resid$subject <- subset_data$subject 

#Move subject column to the very left
vol_resid <- 
  vol_resid %>% 
  dplyr::relocate(subject, .before = Sub_volumes_L)

# For summary, run the vol_resid_func function on the subregion variable
sumvol_resid <- vol_resid_func(subset_data$Cyst_volumes_L)

# Obtain the summary statistics for the linear regression model
# Values are 0 across the board. Perhaps difference will be including entire dataframe.
summary(sumvol_resid)





