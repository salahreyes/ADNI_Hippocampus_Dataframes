#Create variables so that they can be ready for combined dataframe
volresidtime1 <- vol_residscreeningnew
volresidtime2 <- vol_residmonth12new
volresidtime3 <- vol_residmonth24new
volresidtime4 <- vol_residmonth36new
volresidtime5 <- vol_residmonth48new

combined_residuals <- rbind(volresidtime1, volresidtime2, volresidtime3, volresidtime4, volresidtime5)

combined_residuals <- combined_residuals[order(combined_residuals$subject), ]




write.csv(combined_residuals, "combined_residualsnew.csv", row.names = FALSE)
