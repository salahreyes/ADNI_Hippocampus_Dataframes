#Create variables so that they can be ready for combined dataframe
voltime1 <- vol_residscreeningnew
voltime2 <- vol_residmonth12new
voltime3 <- vol_residmonth24new
voltime4 <- vol_residmonth36new
voltime5 <- vol_residmonth48new

# get column names for each data frame
names_voltime1 <- colnames(voltime1)
names_voltime2 <- colnames(voltime2)
names_voltime3 <- colnames(voltime3)
names_voltime4 <- colnames(voltime4)
names_voltime5 <- colnames(voltime5)

# identify different column names
diff_names_1_2 <- setdiff(names_voltime1, names_voltime2)
diff_names_2_3 <- setdiff(names_voltime2, names_voltime3)
diff_names_3_4 <- setdiff(names_voltime3, names_voltime4)
diff_names_4_5 <- setdiff(names_voltime4, names_voltime5)

# print results
print(diff_names_1_2)
print(diff_names_2_3)
print(diff_names_3_4)
print(diff_names_4_5)


combined_volumes <- rbind(voltime1, voltime2, voltime3, voltime4, voltime5)

combined_volumes <- combined_volumes[order(combined_volumes$subject), ]

write.csv(combined_volumes, "combined_residualsnew.csv", row.names = FALSE)
