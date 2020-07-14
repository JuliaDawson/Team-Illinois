#Read Excel File
install.packages("tidyverse")
library(readxl)
#illinois_raw = read_xlsx("C:\Users\julia\OneDrive\Documents\R code\STAT_420\FinalProject\county health\2020 County Health Rankings Illinois Data - v1_0.xlsx", sheet = 1)
illinois_raw = read_xlsx("C:\\Users\\julia\\OneDrive\\Documents\\R code\\STAT_420\\FinalProject\\county_health\\2020 County Health Rankings Illinois Data - v1_0.xlsx", sheet = 4, skip = 1)
illinois_raw = subset(illinois_raw, select = c("FIPS", "State", "County", "Deaths","Years of Potential Life Lost Rate",
"% Fair or Poor Health", "Average Number of Physically Unhealthy Days", "Average Number of Mentally Unhealthy Days", 
"% Low Birthweight", "% Smokers", "% Adults with Obesity", "Food Environment Index", "% Physically Inactive", 
"% With Access to Exercise Opportunities",
"% Excessive Drinking", "# Alcohol-Impaired Driving Deaths", "# Driving Deaths", 
"% Driving Deaths with Alcohol Involvement", "# Chlamydia Cases", "Chlamydia Rate", "Teen Birth Rate", 
"# Uninsured", "% Uninsured", "# Primary Care Physicians", "Primary Care Physicians Rate", 
"Primary Care Physicians Ratio", "# Dentists", "Dentist Rate", "Dentist Ratio", "# Mental Health Providers",
"Mental Health Provider Rate", "Mental Health Provider Ratio", "Preventable Hospitalization Rate",
"% With Annual Mammogram", "% Vaccinated", "Cohort Size", "High School Graduation Rate", 
"# Some College", "Population", "% Some College", "# Unemployed", "Labor Force", "% Unemployed", 
"% Children in Poverty", "80th Percentile Income", "20th Percentile Income", "Income Ratio", 
"# Single-Parent Households", "# Households", "% Single-Parent Households", "# Associations", "Social Association Rate",
"Annual Average Violent Crimes", "Violent Crime Rate", "# Injury Deaths", "Injury Death Rate",
"Average Daily PM2.5", "Presence of Water Violation", "% Severe Housing Problems", "Severe Housing Cost Burden",
"Overcrowding", "Inadequate Facilities", "% Drive Alone to Work", 
"# Workers who Drive Alone", "% Long Commute - Drives Alone"))
str(illinois_raw)
write.csv(illinois_raw, "C:\\Users\\julia\\OneDrive\\Documents\\R code\\STAT_420\\FinalProject\\raw_illinois.csv")
