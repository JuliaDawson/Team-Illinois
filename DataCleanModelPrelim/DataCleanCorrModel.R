#====================================================================================================
# Get correlation between each numeric variable and the p-values corresponding to significance
# of the levels of correlation and store them in a flattened file of data element pairs and these
# two measures, in output file Correlation.xlsx in your home directory.
#
# Input file in your home directory should be nameed: Consolidated_95_Raw.xlsx
# it is downloadable from our github in the zip package: DataAnalysis
#---------------------------------------------------------------------------------------------------
#install.packages("Hmisc")  #Run these the first time...
#install.packages("xlsx")
#install.packages("tidyverse")
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
library("tidyverse")
library("readr") #to read excel file
library("Hmisc") #for rcorr to flatten correlation matrix
#
#options(max.print=1000000)
#getwd() #What is the home directory

#Read Excel File for raw data and then we will add the corrpval sheet too it...
#Read Raw data, sheet 2 with short names in line 1. Also, NA has been replaced by 0 everywhere.
na_strings = c("NA")
Raw95 = readxl::read_excel("C:\\Users\\julia\\OneDrive\\Documents\\R code\\STAT_420\\FinalProject\\Consolidated_95_Raw.xlsx", na = na_strings)
Raw95 = as.data.frame(unclass(Raw95))
names(Raw95) = c("Row",
                 "State",
                 "Region",
                 "FIPS",
                 "Deaths",
                 "YPLLRate",
                 "HealthPoorPct",
                 "HealthBadDaysPhysAvg",
                 "HealthBadDaysMentAvg",
                 "LBWPct",
                 "SmokersPct",
                 "ObeseAdultsPct",
                 "FoodEnvirIX",
                 "PhysInactPct",
                 "ExerAccPct",
                 "DrinkExcPct",
                 "DeathsDrinkDrivePct",
                 "ChlamydiaRate",
                 "TeenBirthRate",
                 "UninsuredPct",
                 "PhysPrimCareRate",
                 "PhysPrimCareRatio",
                 "DentistRate",
                 "DentistRatio",
                 "MentHProvRate",
                 "MentHProvRatio",
                 "HospPrevRate",
                 "MammAnnualPct",
                 "VaccinatedPct",
                 "CohortSize",
                 "HSGradRate",
                 "Population",
                 "CollegeSomePct",
                 "LaborForce",
                 "UnemployedPct",
                 "ChildInPovertyPct",
                 "Income80thpcntl",
                 "Income20thpcntl",
                 "IncomeRatio",
                 "SingsParsHshsldsPct",
                 "SocsAssRate",
                 "ViolsCrimeAnlAvg",
                 "ViolCrimeRate",
                 "InjuryDeathRate",
                 "DailyPM25Avg",
                 "WaterViol",
                 "HousSvrProbPct",
                 "HousSvrCostBurden",
                 "HousSvrOvercrowd",
                 "HousInadFacil",
                 "DriveAloneWorkPct",
                 "DriveAloneLongPct",
                 "FIPS2",
                 "LifeExpect",
                 "DeathAgeAdjRate",
                 "DeathChildRate",
                 "DeathInfantRate",
                 "DistressFreqPhysPct",
                 "DistressFreqMentPct",
                 "DiabetesAdultPct",
                 "HIVRate",
                 "FoodInsecurePct",
                 "FoodHealthLimAccPct",
                 "DeathDrugODRate",
                 "DeathMVRate",
                 "SleepPoorPct",
                 "UninsAdultPct",
                 "UninsChildPct",
                 "OthPrimCareProvRate",
                 "OthPrimCareProvRatio",
                 "DisconnYouthPct",
                 "AvgGradeRead",
                 "AvgGradeMath",
                 "IncHousehldMedian",
                 "LunchFreeReducedPct",
                 "SegIXBlackWhite",
                 "SegIXnonWhiteWhite",
                 "HomicideRate",
                 "SuicAgeAdjRate",
                 "SuicCrudeRate",
                 "DeathsFireArmsRate",
                 "ArrestJuveNonPet",
                 "ArrestJuvePet",
                 "ArrestJuveDenom",
                 "ArrestJuveRate",
                 "TrafficVolume",
                 "HomeownersPct",
                 "HousehldSvrCostPct",
                 "Population2",
                 "PopLT18Pct",
                 "PopGE65Pct",
                 "PopNativePct",
                 "PopPacificPct",
                 "EnglishNotProfPct",
                 "PopFemalePct",
                 "PopRuralPct"
)
# is Population 2 same as Population and FIPS2 same as FIPS?
all.equal(Raw95$FIPS, Raw95$FIPS2) #yes are same
all.equal(Raw95$Population, Raw95$Population2) #Population2 is about four times as large
lookat = cbind(Raw95$Population, Raw95$Population2)
head(lookat)
#Begin drop Row, drop ratios that are characters (they have more useful rates). IncomeRatio is a numeric so keep it.
Raw95 = subset(Raw95, select = -c( Row, PhysPrimCareRatio, DentistRatio, MentHProvRatio, FIPS2, OthPrimCareProvRatio))
str(Raw95)
#Force character arrays to be factors.  If necessary convert them to character first.
Raw95$State = as.factor(Raw95$State)
#is.factor(Raw95$State)
Raw95$Region= as.factor(Raw95$Region)
#is.factor(Raw95$Region)
Raw95$WaterViol = as.factor(Raw95$WaterViol)
#is.factor(Raw95$WaterViol)
str(Raw95)

#Begin dealing with NA =================================================================================================
NAthreshhold = 294 #count of NAs above which we drop the data element
NAcount = colSums(is.na(Raw95))
j = 0
for (i in 1:length(Raw95)) {
   if (NAcount[i] < NAthreshhold) { #generates a list of names to keep based on count of NA below threshhold
     cat(names(NAcount[i]), ", ", sep="")
     j = j + 1
   }
}

cat(paste(j, "data elements have NA counts below the threshhold of ", NAthreshhold)) #Number of data elements to keep because count of NA below threshhold

#drop data elements with counts of NA that exceed threshhold, paste from console output of previous section
Raw95nadrop = subset(Raw95, select = c(State, Region, FIPS, Deaths, YPLLRate, HealthPoorPct, HealthBadDaysPhysAvg, HealthBadDaysMentAvg, LBWPct, SmokersPct, ObeseAdultsPct, FoodEnvirIX, PhysInactPct, ExerAccPct, DrinkExcPct, DeathsDrinkDrivePct, ChlamydiaRate, TeenBirthRate, UninsuredPct, PhysPrimCareRate, DentistRate, MentHProvRate, HospPrevRate, MammAnnualPct, VaccinatedPct, HSGradRate, Population, CollegeSomePct, LaborForce, UnemployedPct, ChildInPovertyPct, Income80thpcntl, Income20thpcntl, IncomeRatio, SingsParsHshsldsPct, SocsAssRate, ViolsCrimeAnlAvg, ViolCrimeRate, InjuryDeathRate, DailyPM25Avg, WaterViol, HousSvrProbPct, HousSvrCostBurden, HousSvrOvercrowd, HousInadFacil, DriveAloneWorkPct, DriveAloneLongPct, LifeExpect, DeathAgeAdjRate, DistressFreqPhysPct, DistressFreqMentPct, DiabetesAdultPct, FoodInsecurePct, FoodHealthLimAccPct, SleepPoorPct, UninsAdultPct, UninsChildPct, OthPrimCareProvRate, IncHousehldMedian, LunchFreeReducedPct, TrafficVolume, HomeownersPct, HousehldSvrCostPct, Population2, PopLT18Pct, PopGE65Pct, PopNativePct, PopPacificPct, EnglishNotProfPct, PopFemalePct, PopRuralPct))

str(NAcount)
length(Raw95) #Number of data elements, 92
nrow(Raw95)  #Number of observations !!!originally 3142!!!
sum(is.na(Raw95))
mean(is.na(Raw95))
Raw95naomit = na.omit(Raw95nadrop)
sum(is.na(Raw95naomit))
mean(is.na(Raw95naomit))
length(Raw95naomit) #Number of data elements, 71 
nrow(Raw95naomit)  #Number of observations !!!have 2333 left!!!

#Also make one without State and Region - drop data elements with counts of NA that exceed threshhold, paste from console output of previous section
Raw95nostate = subset(Raw95naomit, select = c(FIPS, Deaths, YPLLRate, HealthPoorPct, HealthBadDaysPhysAvg, HealthBadDaysMentAvg, LBWPct, SmokersPct, ObeseAdultsPct, FoodEnvirIX, PhysInactPct, ExerAccPct, DrinkExcPct, DeathsDrinkDrivePct, ChlamydiaRate, TeenBirthRate, UninsuredPct, PhysPrimCareRate, DentistRate, MentHProvRate, HospPrevRate, MammAnnualPct, VaccinatedPct, HSGradRate, Population, CollegeSomePct, LaborForce, UnemployedPct, ChildInPovertyPct, Income80thpcntl, Income20thpcntl, IncomeRatio, SingsParsHshsldsPct, SocsAssRate, ViolsCrimeAnlAvg, ViolCrimeRate, InjuryDeathRate, DailyPM25Avg, WaterViol, HousSvrProbPct, HousSvrCostBurden, HousSvrOvercrowd, HousInadFacil, DriveAloneWorkPct, DriveAloneLongPct, LifeExpect, DeathAgeAdjRate, DistressFreqPhysPct, DistressFreqMentPct, DiabetesAdultPct, FoodInsecurePct, FoodHealthLimAccPct, SleepPoorPct, UninsAdultPct, UninsChildPct, OthPrimCareProvRate, IncHousehldMedian, LunchFreeReducedPct, TrafficVolume, HomeownersPct, HousehldSvrCostPct, Population2, PopLT18Pct, PopGE65Pct, PopNativePct, PopPacificPct, EnglishNotProfPct, PopFemalePct, PopRuralPct))
#Plot pairs of top fit (***) for both full and backfit model
Raw95TopFit = subset(Raw95naomit, select = c(YPLLRate, FIPS, State, DeathAgeAdjRate,      
                                             EnglishNotProfPct,    
                                             HousInadFacil,       
                                             Income20thpcntl,      
                                             InjuryDeathRate,     
                                             LBWPct,               
                                             LifeExpect,           
                                             MammAnnualPct,        
                                             PopFemalePct,         
                                             PopGE65Pct,          
                                             PopNativePct,         
                                             PopRuralPct,          
                                             SocsAssRate         
))        
pairs(Raw95TopFit)

#====GLM==========================================================================================================
#glmall - Try glm without Region - AIC 35295, 
model_glmall = glm(YPLLRate ~ . -Region, data= Raw95naomit)
summary(model_glmall)
plot(model_glmall)

#reject nostate - glm without State and Region - higher AIC and dispersion parameter
model_nostate = glm(YPLLRate ~ . , data= Raw95nostate)
summary(model_nostate)
plot(model_nostate)

#glmallback - Work back from glmall
mdl_back = step(model_glmall, trace = 0, maxit = 50)
plot(mdl_back)
summary(mdl_back)

#topfit - picked only 3 stars from both the glmall and glmallback
model_topfit = lm(YPLLRate ~ ., data= Raw95TopFit)
plot(model_topfit)
summary(model_topfit)

# Try a different Result
#glmall - Try glm without Region - AIC 35295, 
model_glmexper = glm(YPLLRate ~ . -Region, data= Raw95naomit)
summary(model_glmexper)
plot(model_glmexper)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

mdl_lmnostate = lm(formula = YPLLRate ~ HealthBadDaysMentAvg + LBWPct + 
      FoodEnvirIX + PhysInactPct + DeathsDrinkDrivePct + ChlamydiaRate + 
      UninsuredPct + PhysPrimCareRate + HospPrevRate + MammAnnualPct + 
      VaccinatedPct + Population + CollegeSomePct + Income20thpcntl + 
      IncomeRatio + SocsAssRate + InjuryDeathRate + DailyPM25Avg + 
      HousSvrProbPct + HousSvrCostBurden + HousSvrOvercrowd + HousInadFacil + 
      DriveAloneWorkPct + DriveAloneLongPct + LifeExpect + DeathAgeAdjRate + 
      DistressFreqMentPct + UninsAdultPct + IncHousehldMedian + 
      Population2 + PopGE65Pct + PopNativePct + PopPacificPct + 
      EnglishNotProfPct + PopFemalePct + PopRuralPct, data = Raw95naomit)
summary(mdl_lmnostate)
plot(mdl_lmnostate)

Raw95back = subset(Raw95naomit, select = c(YPLLRate, HealthBadDaysMentAvg, LBWPct, 
                       FoodEnvirIX, PhysInactPct, DeathsDrinkDrivePct, ChlamydiaRate, 
                       UninsuredPct, PhysPrimCareRate, HospPrevRate, MammAnnualPct, 
                       VaccinatedPct, Population, CollegeSomePct, Income20thpcntl, 
                       IncomeRatio, SocsAssRate, InjuryDeathRate, DailyPM25Avg, 
                       HousSvrProbPct, HousSvrCostBurden, HousSvrOvercrowd, HousInadFacil, 
                       DriveAloneWorkPct, DriveAloneLongPct, LifeExpect, DeathAgeAdjRate, 
                       DistressFreqMentPct, UninsAdultPct, IncHousehldMedian, 
                       Population2, PopGE65Pct, PopNativePct, PopPacificPct, 
                       EnglishNotProfPct, PopFemalePct, PopRuralPct))

pairs(Raw95back)


#Begin Correlations =====================================================================================================
#Pairs plot is not working for so many, but use later to see if need to transform...
#pairs(select_if(Raw95, is.numeric)) #tried visual plot but seems to have failed.  ??? dplyr should have been enough

corr_matrix = round(cor(select_if(Raw95back, is.numeric) , use="pairwise.complete.obs"),2) #if non-numeric need pairwise.complete
head(corr_matrix)
#
# Stores both Correlation and p-values corresponding to significance levels of correlations
# per http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
#res2 = rcorr(corr_matrix, type = c("pearson", "spearman"))
#Extract the correlation coefficients
#res2$r
#Extract the p-values
#res2$P
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

#pairwise.complete.obs needed if non numeric dat is in the matrix
res2<-rcorr(as.matrix(cor(select_if(Raw95, is.numeric) , use="pairwise.complete.obs")), type = c("pearson", "spearman"))
#Flatten from a square matrix to each pair of data elements with their correlation and p-value corresponding to significance levels of correlations
flat_matrix = flattenCorrMatrix(res2$r, res2$P)
head(flat_matrix)
openxlsx::write.xlsx(x = flat_matrix,                       # Write xlsx in R
                     file = "Consolidated_95_CorPval.xlsx",
                     sheetName = "CorrPvals", 
                     colnames = TRUE, rownames = TRUE, append = FALSE)

#End Correlations================================================================================================

??caret

# Elastic Net Experiment ========================================================================================
y = as.matrix(Raw95naomit$YPLLRate)
#Only include those that have non-zero var.  Many have NA.  Var did not like factors either 
x = as.matrix(subset(Raw95naomit, select = -c(YPLLRate)))
#enet says this about above: Error in one %*% x : requires numeric/complex matrix/vector arguments
x = as.matrix(subset(Raw95naomit, select = -c(State, Region, YPLLRate, WaterViol)))
#Now it works :} don't need to get rid of var=0 data after all...
head(x)
is.matrix(x)
is.matrix(y)
head(x)
head(y)
#Error in t(y) %*% x : non-conformable arguments   

#Try enet - R elastic net  https://cran.r-project.org/web/packages/elasticnet/elasticnet.pdf
library(elasticnet)
library(lars)
#??elasticnet
object = enet(x, y, lambda=1)
par(mfrow=c(2,2))
plot(object)
plot(object,xvar="step")
predict(object, type = c("coefficients"), naive=TRUE)
print(enet)

