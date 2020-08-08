library(tidyverse)
library(readr)
library(dplyr)
library(faraway)
library(lmtest)
library(MASS)
library(knitr)


options(digits = 3)

getwd()
Raw95naomit = read.csv("Model_3_data.csv") 
str(Raw95naomit)

calc_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

#Force character arrays to be factors.  
Raw95naomit$State = as.factor(Raw95naomit$State)
Raw95naomit$Region= as.factor(Raw95naomit$Region)
Raw95naomit$WaterViol = as.factor(Raw95naomit$WaterViol)

#Make dataframe just with used data elements for later use in weight and scale calculations
Raw95just14 = subset(Raw95naomit, select = c(YPLLRate,HealthPoorPct, FoodEnvirIX, PhysInactPct, PhysPrimCareRate, 
                                             MammAnnualPct, Income20thpcntl,IncomeRatio,SocsAssRate,InjuryDeathRate,HousSvrProbPct,HousInadFacil,
                                             LifeExpect,DeathAgeAdjRate,VaccinatedPct,IncHousehldMedian,PopGE65Pct,PopNativePct,PopPacificPct,EnglishNotProfPct, PopRuralPct,PopGE65Pct:DeathAgeAdjRate,PopFemalePct,LBWPct,IncHousehldMedian)) 
Raw95just14 = as.data.frame(Raw95just14)


# Train-Test logic: Split Data into Train and Test and Compare RMSE and percent error:

#Eliminate influential points from beginning so can split into train and test.
influential = which(cdnaomit > 4/length(cdnaomit))
Raw95nooutlier = Raw95just14[-influential,]

get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

get_perc_err = function(actual, predicted) {
  100 * mean((abs(actual - predicted)) / actual)
}

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
  sdtrboxcox = sd(Raw95Train$YPLLRate^lmda_mdljd)
  
  #plain: This model has the response BoxCox transform
  mdljdfinal_trn = lm(formula = YPLLRate^lmda_mdljd ~ HealthPoorPct + 
                        FoodEnvirIX + PhysInactPct + 
                        PhysPrimCareRate + 
                        MammAnnualPct +   
                        Income20thpcntl + IncomeRatio + SocsAssRate + 
                        InjuryDeathRate + HousSvrProbPct + 
                        HousInadFacil + LifeExpect + 
                        DeathAgeAdjRate + VaccinatedPct + 
                        IncHousehldMedian + 
                        PopGE65Pct + PopNativePct + PopPacificPct + 
                        EnglishNotProfPct + PopRuralPct
                      + PopGE65Pct:DeathAgeAdjRate
                      + HealthPoorPct:PopFemalePct
                      + LBWPct:IncHousehldMedian
                      , data = Raw95Train)
  

  rmse_final[i]   = get_loocv_rmse(mdljdfinal_trn)
  ratio_final[i]  = get_loocv_rmse(mdljdfinal_trn)/sdtrboxcox
  #Prediction with Test Values
  pcterr_final[i] = get_perc_err(Raw95Test$YPLLRate^lmda_mdljd, predict(mdljdfinal_trn, Raw95Test))
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

