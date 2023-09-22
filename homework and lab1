#1. Mishal Nawaz and Vanessa Crowe

#2. signed up for hawkes, reviewing ebook

#3. baking
#results for 20 rolls (non-sixes are re)
#die1: 6 1 2 6 6 1 1 6 6 1 1 4 6 6 6 6 6 1 6 6
#die2: 6 3 6 6 6 4 5 6 4 1 3 1 1 6 1 3 6 6 4 6
#6's were rolled 14 times

#4.

library(dplyr)
library(plotly)
library(tidyverse)

load("Household_Pulse_data_w57.RData")
summary(Household_Pulse_data)
Household_Pulse_data[1:10,1:6]
attach(Household_Pulse_data)

summary(Household_Pulse_data)
view(Household_Pulse_data)

#Comparison
# minimum and maximum ages are the same among all genders but the differences between the quartiles were clear
#Specifically, you can see that the transgender individuals surveyed were younger than all other genders
#transgender individuals included were born in the 80s and 90s while the other genders did not include those born in the 90s
#the mean for transgender surveyees was the only mean that was not an exact match to the summary and it
#also had the lowest standard deviation

summary(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
summar(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
summary(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])

#average ages for men and women

mean(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "female"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "male"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "transgender"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "other"])
mean(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])
sd(TBIRTH_YEAR[GENID_DESCRIBE == "NA"])

##All of the amounts, no matter the kids ages had answered similarly "very stressed about price changes"
#The similarities under all the answers were interesting because I initially thought households with younger
# people would be more stressed about price changes

summary(PRICESTRESS[KIDS_12_17Y == "Yes children 12 - 17 in HH"])
summary(PRICESTRESS[KIDS_5_11Y == "Yes children 5 - 11 in HH"])
summary(PRICESTRESS[KIDS_LT5Y == "Yes children under 5 in HH"])


#the 'south' and the 'west' regions had the more 'no's' for long covid and 'hadcovid' but they also had the
#most for "had symptoms 3mo or more" / "yes tested + or doc told me had covid." This could be that
#the majority of the residents surveyed were in those regions so the data will always be skewed 

summary(LONGCOVID[REGION == "South"])
summary(LONGCOVID[REGION == "Midwest"])
summary(LONGCOVID[REGION == "West"])
summary(LONGCOVID[REGION == "Northeast"])

summary(HADCOVIDRV[REGION == "South"])
summary(HADCOVIDRV[REGION == "West"])
summary(HA\DCOVIDRV[REGION == "Midwest"])
summary(HADCOVIDRV[REGION == "Northeast"])

#I was most curious about the experiences of longcovid and the region was a new way to understand. I noticed
#that the number of people who had covid compared to those who clicked they had long covid, had similar ratios 
#no matter the location
