

# Questão 1 ----
i_rad_sol=c(190, 118, 149, 313, 299, 99, 256, 290, 274, 65, 334, 307, 78, 322, 44, 8, 320, 25, 92, 13)

v_vento=c(7.4, 8, 12.6, 11.5, 8.6, 13.8, 9.7, 9.2, 10.9, 13.2, 11.5, 12, 18.4, 11.5, 9.7, 9.7, 16.6, 9.7, 12, 12)

temp=c(67, 72, 74, 62, 65, 59, 69, 66, 68, 58, 64, 66, 57, 68, 62, 59, 73, 61, 61, 67)

ozonio=c(41, 36, 12, 18, 23, 29, 16, 11, 14, 18, 14, 34, 6, 30, 11, 1, 11, 4, 32, 23)

quali_ar = base::cbind(i_rad_sol, v_vento, temp, ozonio)|>base::as.data.frame()

quali_ar$i_rad_sol|>mean()

mean(quali_ar)

base::apply(X = quali_ar, MARGIN =  2, FUN = mean)

stats::cor(x = quali_ar, use = "all.obs", method = "pearson")
stats::cov(x = quali_ar, use = "all.obs", method = "pearson")










