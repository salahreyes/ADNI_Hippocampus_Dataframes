#Create variables so that they can be ready for combined dataframe
volresidtime1 <- vol_residscreening
volresidtime2 <- vol_residmonth12
volresidtime3 <- vol_residmonth24
volresidtime4 <- vol_residmonth36
volresidtime5 <- vol_residmonth48

combined_residuals <- rbind(volresidtime1, volresidtime2, volresidtime3, volresidtime4, volresidtime5)

combined_residuals <- combined_residuals[order(combined_residuals$subject), ]

combined_residuals <- cbind(combined_residuals,
                            DxCURREN = combined_volumes$DxCURREN,
                            GDS_Total_Score = combined_volumes$GDS_Total_Score)

write.csv(combined_residuals, "combined_residuals.csv", row.names = FALSE)
