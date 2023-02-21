#load relevant libraries
library(data.table)
library(dplyr)
#search tsv files, put into file_list
file_list <- list.files(path = "/Volumes/LeCie/HippunFold_Outputs/Screening", 
                        pattern = "*volumes*.tsv", 
                        full.names = TRUE, 
                        recursive = TRUE)
#optional, show file_list
file_list

#set up an empty dataframe called 'volume_dt'
volumes_dt <- data.frame(NULL)

#loop that binds tsv values into dataframe of 'volume_dt'
for(file in file_list){
  temp_dt <- fread(file, na.strings = c("NA", "NaN", "", "?"))
  volumes_dt <- bind_rows(volumes_dt, temp_dt)
  gc()
}

#look at dataframe (optional)
volumes_dt

#shift 
volumes_dt <- volumes_dt %>%
  tidyr::pivot_wider(names_from = hemi, values_from = c("Sub", 
                                                        "CA1", 
                                                        "CA2", 
                                                        "CA3", 
                                                        "CA4", 
                                                        "DG", 
                                                        "SRLM", 
                                                        "Cyst")) %>%

volumes_dt  

