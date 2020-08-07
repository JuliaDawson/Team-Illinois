library("readr") #to read excel file
getwd() #What is the home directory
#Read Excel File for raw data and then we will add the corrpval sheet too it...
#Read Raw data, sheet 2 with short names in line 1. Also, NA has been replaced by 0 everywhere.
na_strings = c("NA")
#Raw95 = readxl::read_excel("C:\\Users\\Owner\\OneDrive\\Documents\\R Code\\STAT_420\\FinalProject\\DataCleanModelPrelim\\Consolidated_95_Raw.xlsx", na = na_strings)
Raw95 = readxl::read_excel("Consolidated_95_Raw.xlsx", na = na_strings)
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
#all.equal(Raw95$FIPS, Raw95$FIPS2) #yes are same
#all.equal(Raw95$Population, Raw95$Population2) #Population2 is about four times as large
lookat = cbind(Raw95$Population, Raw95$Population2)
#head(lookat)
#Begin drop Row, drop ratios that are characters (they have more useful rates). IncomeRatio is a numeric so keep it.
Raw95 = subset(Raw95, select = -c( Row, PhysPrimCareRatio, DentistRatio, MentHProvRatio, FIPS2, OthPrimCareProvRatio))
#str(Raw95)
#Force character arrays to be factors.  If necessary convert them to character first.
Raw95$State = as.factor(Raw95$State)
#is.factor(Raw95$State)
Raw95$Region= as.factor(Raw95$Region)
#is.factor(Raw95$Region)
Raw95$WaterViol = as.factor(Raw95$WaterViol)
#is.factor(Raw95$WaterViol)
#str(Raw95)

#Begin dealing with NA ==============================================================================================
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

#Keep data elements with counts of NA below threshhold, paste from console output of previous section
Raw95nakeep = subset(Raw95, select = c(State, Region, FIPS, Deaths, YPLLRate, HealthPoorPct, HealthBadDaysPhysAvg, HealthBadDaysMentAvg, LBWPct, SmokersPct, ObeseAdultsPct, FoodEnvirIX, PhysInactPct, ExerAccPct, DrinkExcPct, DeathsDrinkDrivePct, ChlamydiaRate, TeenBirthRate, UninsuredPct, PhysPrimCareRate, DentistRate, MentHProvRate, HospPrevRate, MammAnnualPct, VaccinatedPct, HSGradRate, Population, CollegeSomePct, LaborForce, UnemployedPct, ChildInPovertyPct, Income80thpcntl, Income20thpcntl, IncomeRatio, SingsParsHshsldsPct, SocsAssRate, ViolsCrimeAnlAvg, ViolCrimeRate, InjuryDeathRate, DailyPM25Avg, WaterViol, HousSvrProbPct, HousSvrCostBurden, HousSvrOvercrowd, HousInadFacil, DriveAloneWorkPct, DriveAloneLongPct, LifeExpect, DeathAgeAdjRate, DistressFreqPhysPct, DistressFreqMentPct, DiabetesAdultPct, FoodInsecurePct, FoodHealthLimAccPct, SleepPoorPct, UninsAdultPct, UninsChildPct, OthPrimCareProvRate, IncHousehldMedian, LunchFreeReducedPct, TrafficVolume, HomeownersPct, HousehldSvrCostPct, Population2, PopLT18Pct, PopGE65Pct, PopNativePct, PopPacificPct, EnglishNotProfPct, PopFemalePct, PopRuralPct))

cat(paste("Raw95: Has ", length(Raw95), 
          " columns and ", nrow(Raw95), " rows." ))
cat(paste("Raw95: Has ", sum(is.na(Raw95)), 
          " NA at rate ", mean(is.na(Raw95))))

#Drop observations with NA in them.
Raw95naomit = na.omit(Raw95nakeep) #drop all of the NA
Raw95naomit = as.data.frame(Raw95naomit) #make a dataframe so can write.csv
str(Raw95naomit)

cat(paste("Raw95naomit: Dropped high na columns, omitted rows with NA, have ", length(Raw95naomit), 
          " columns and ", nrow(Raw95naomit), " rows." ))
cat(paste("Raw95naomit: Has ", sum(is.na(Raw95naomit)), 
          " NA at rate ", mean(is.na(Raw95naomit))))

#Write CSV file out
write.csv(Raw95naomit, ".\\Data\\DataCleanedJulia.csv", row.names = FALSE)
