get_perc_err = function(actual, predicted) {
  100 * mean((abs(actual - predicted)) / actual)
}

cdnaomit = cooks.distance(mdl3t)
influential = which(cdnaomit > 4/length(cdnaomit))
Raw95nooutlier = Raw95naomit[-influential,]

set.seed(42)
num_sim = 1000

rmse_final = c(rep(num_sim, 0))
ratio_final = c(rep(num_sim, 0))
pcterr_final = c(rep(num_sim, 0))

for (i in 1:num_sim) {
  indexes = sample(nrow(Raw95nooutlier), (nrow(Raw95nooutlier)) /2)
  Raw95Train = Raw95nooutlier[indexes,]
  Raw95Test = Raw95nooutlier[-indexes,]
  
  #...dont need subset because already got rid of influential points
  #final: Calculate standard deviations for train, and for YPLLRate and YPLLRate^lmda_mdljd
  sdtrboxcox = sd(Raw95Train$YPLLRate^lmda_mdl3)
  
  #plain: This model has the response BoxCox transform
  mdljdfinal_trn = lm(formula = formula(model_3), data = Raw95Train)
  
  rmse_final[i]   = get_loocv_rmse(mdljdfinal_trn)
  ratio_final[i]  = get_loocv_rmse(mdljdfinal_trn)/sdtrboxcox
  
  #Prediction with Test Values
  pcterr_final[i] = get_perc_err(Raw95Test$YPLLRate^lmda_mdl3, predict(mdljdfinal_trn, Raw95Test))
}


par(mfrow = c(1, 2))
hist(ratio_final,
     xlab   = "RMSE/StdDev",
     main   = "Ratio RMSE and StdDev\nModel Boxcox, No Outliers",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 45)

hist(pcterr_final,
     xlab   = "Percent Error",
     main   = "Percent Error\nModel Boxcox, No Outliers",
     col    = "darkorange",
     border = "dodgerblue",
     breaks = 45)

median(pcterr_final)
mean(pcterr_final)

median(ratio_final)
mean(ratio_final)