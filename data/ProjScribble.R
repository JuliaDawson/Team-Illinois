#install.packages("tidyverse")
#install.packages("dplyr")

library(tidyverse)
library(readxl)
library(dplyr)

# Read files name to iterate over later
filelist = dir("county health/", full.names = T) 

###############################
## Tab 4 Ranked Measure Data ##
###############################
consolidated_file_name_MD = "data//Consolidated_RankedMeasureData.csv"
ConsdDF = lapply(filelist, function(filename){
  print(paste("Merging",filename,sep = " "))
  read_excel(filename, sheet = "Ranked Measure Data", skip = 1)
  })
df = bind_rows(ConsdDF)
df = select(df, -contains("Z-Score"))
df = select(df, -contains("95% CI"))
#df$County <- paste(df$County,df$Borough)
#df %>% unite(Region, "County":"Borough", remove = TRUE, na.rm = TRUE)

df = df %>% unite(Parish_Borough, Parish, Borough, remove = FALSE, na.rm = TRUE)
df = df %>% unite(Region, County, Parish_Borough, sep = "", remove = FALSE, na.rm = TRUE)
df = select(df, -contains("Parish_Borough"))
df = select(df, -contains("Parish"))
df = select(df, -contains("Borough"))

# write to a CSV
write.csv(df, consolidated_file_name_MD)

#################################
# Tab 5 Additional Measure Data #
#################################
consolidated_file_name_AMD = "data//Consolidated_AdditionalMeasureData.csv"
ConsdAMD = lapply(filelist, function(filename){
  print(paste("Merging",filename,sep = " "))
  read_excel(filename, sheet = "Additional Measure Data", skip = 1, col_types = "text")

})
#amd_df = do.call(rbind.data.frame, ConsdAMD)
amd_df = bind_rows(ConsdAMD)
amd_df = select(amd_df, -contains("Z-Score"))
amd_df = select(amd_df, -contains("95% CI"))
amd_df = amd_df %>% unite(Parish_Borough, Parish, Borough, remove = FALSE, na.rm = TRUE)
amd_df = amd_df %>% unite(Region, County, Parish_Borough, sep = "", remove = FALSE, na.rm = TRUE)
amd_df = select(amd_df, -contains("Parish_Borough"))
amd_df = select(amd_df, -contains("Parish"))
amd_df = select(amd_df, -contains("Borough"))

write.csv(amd_df, consolidated_file_name_AMD)

########################################
# Tab 2 - Outcomes & Factors Rankings ##
########################################
consolidated_file_name_Rank = "data//Consolidated_Outcomes_Factors_Rankings.csv"
ConsdRankDF = lapply(filelist, function(filename){
  print(paste("Merging",filename,sep = " "))
  read_excel(filename, sheet = "Outcomes & Factors Rankings", skip = 1, col_types = "text")
})
dfr = bind_rows(ConsdRankDF)
dfr = dfr %>% unite(Parish_Borough, Parish, Borough, remove = FALSE, na.rm = TRUE)
dfr = dfr %>% unite(Region, County, Parish_Borough, sep = "", remove = FALSE, na.rm = TRUE)
dfr = select(dfr, -contains("Parish_Borough"))
dfr = select(dfr, -contains("Parish"))
dfr = select(dfr, -contains("Borough"))
# write to a CSV
write.csv(dfr, consolidated_file_name_Rank)



#####################################
#### Read the consolidated files #### 
#####################################

outcomes_csv = read_csv("Consolidated_Outcomes_Factors_Rankings.csv") 
rank_csv = read_csv("Consolidated_RankedMeasureData.csv")
additional_csv = read_csv("Consolidated_AdditionalMeasureData.csv") 

#intermediate_join = merge(outcomes_csv, rank_csv, by=c("State","Region"))
#all_the_DF = merge(intermediate_join, additional_csv, by=c("State","Region"))

all_the_DF = merge(rank_csv, additional_csv, by=c("State","Region"))

# Some additional clean up 
all_the_DF = select(all_the_DF, -contains("County"))
all_the_DF = select(all_the_DF, -contains("Asian"))
all_the_DF = select(all_the_DF, -contains("Black"))
all_the_DF = select(all_the_DF, -contains("AIAN"))
all_the_DF = select(all_the_DF, -contains("White"))
all_the_DF = select(all_the_DF, -contains("Hispanic"))
all_the_DF = select(all_the_DF, -contains("X1"))
all_the_DF = select(all_the_DF, -contains("#"))
all_the_DF = select(all_the_DF, -contains("Z-Score"))
all_the_DF = select(all_the_DF, -contains("Unreliable"))
all_the_DF = subset(all_the_DF, all_the_DF$Region != "NA")

write.csv(all_the_DF, "Consolidated_95_observations.csv")


# FINAL COUNT is 3142 observations, 95 predictors
proposal_csv = read_csv("Consolidated_95_observations.csv") 
head(proposal_csv)






