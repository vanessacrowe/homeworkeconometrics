#Lab 3

library(dplyr)
library(plotly)
library(tidyverse)

load("Household_Pulse_data_w57.RData")
summary(Household_Pulse_data)

summary(RECVDVACC == 'yes got vaxx')
summary(RECVDVACC == 'no did not get vaxx')
summary(RECVDVACC == 'NA')

Household_Pulse_data(RECVDVACC)

summary(RECVDVACC == "yes got vaxx")
summary(RECVDVACC)

#those who got vaxx and had no employment in the last 7 days - 30.7%

restrict1 <- (Household_Pulse_data$RECVDVACC == 'yes got vaxx') & (Household_Pulse_data$ANYWORK == "no employment in last 7 days")
data_new1 <- subset(Household_Pulse_data,restrict1)

view(restrict1)
summary(restrict1)
Mode   FALSE    TRUE 
logical   41068   18222 

#those who did not get vaxxed and had employment in the last 7 days - 7.9%

restrict2 <- (Household_Pulse_data$RECVDVACC == 'no did not get vaxx') & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days")
data_new2 <- subset(Household_Pulse_data,restrict2)
summary(restrict2)
Mode   FALSE    TRUE 
logical   54610    4680 

#those who did got vaxxed and had employment in the last 7 days - 53.6%

restrict3 <- (Household_Pulse_data$RECVDVACC == 'yes got vaxx') & (Household_Pulse_data$ANYWORK == "yes employment in last 7 days")
data_new3 <- subset(Household_Pulse_data,restrict3)
summary(restrict3)
Mode   FALSE    TRUE 
logical   27534   31756 

#those who did not get vaxx and had no employment in the last 7 days - 4.4%

restrict4 <- (Household_Pulse_data$RECVDVACC == 'no did not get vaxx') & (Household_Pulse_data$ANYWORK == "no employment in last 7 days")
data_new4 <- subset(Household_Pulse_data,restrict4)
summary(restrict4)
Mode   FALSE    TRUE 
logical   56694    2596 

#there is a larger percentage of one who has a job an received the vaccination which could be because many jobs required vaccination proof
#however, there is also a large percentage of individuals who got the vaccination and had no employment; this is considering the fact that in the
#beginning of the pandemic, many people lost jobs
#these two variables were not as straightforward as I imagined them to be as there are too many  factors behind why people who got vaxxed were 
#not employed vs those who did not who were employed

#do you want to limit the comparisons further? for example people who are not vaxxed less likely to get their kids
restrict5 <- (Household_Pulse_data$RECVDVACC == 'no did not get vaxx') & (Household_Pulse_data$KIDGETVAC_LT5Y == "kids under 5yo definitely NOT get vaxx")
data_new5 <- subset(Household_Pulse_data,restrict5)
summary(restrict5)
Mode   FALSE    TRUE 
logical   53141    6149 

#10.4% of those who did not get vaxxed did not vaccinate their kids

restrict6 <- (Household_Pulse_data$RECVDVACC == 'no did not get vaxx') & (Household_Pulse_data$KIDGETVAC_LT5Y == "kids under 5yo definitely get vaxx")
data_new6 <- subset(Household_Pulse_data,restrict6)
summary(restrict6)
Mode   FALSE    TRUE 
logical   59285       5 

#.0084% who did not get vaxxed vaccinated their kids

#what is reasonable comparing the reasonable groups, what is the size difference of the outcome?

#I chose children under 5Years old because younger children were more susceptable to covid so I assumed parents would be more weary of the virus and possibly
#chose to vaccinate their child compared to their teenager however, i still observed that those who did not get vaxxed were less likely to have their child vaxxed.


#what is the standard error of that difference measure? 
#using values from the true statement(6149) and the population (59290); the standard error of those whos kids did not get vaxxed is .00125
#using value from the true statement(5) and the population (59290); the standard error of those whos kids got vaxxed is .0000377

#using stats knowledge, how confident are you, that the difference is actually there and not an arefact of sampling?

#this shows that there is a very small amount of variability in the estimation that people who did not get vaxxed are less likely to vaxx their kids
#i do believe I can be confident about that there is little uncertainty among the sample

#look at the crosstabs and compute marginal possibilities, how do those inform?
#To check if you did it right, compute some of the marginal probabilities using Bayes' theorem.
#Is your crosstab mutually exclusive and exhaustive?
#What other factors could explain the difference in outcome?
#Among your list of differences in vaxx status, are there some potential confounders such as age or education? What else?
#What additional evidence would you look at? What conclusions could you draw from that?
#How confident would you be, in the conclusions made? 
#What other conclusions could be drawn, from that same evidence?
#If you were to try to persuade someone, imagine what evidence would be required to persuade a person with the opposite view?

table1 <- table(Household_Pulse_data$RECVDVACC, Household_Pulse_data$KIDGETVAC_LT5Y)
print(table1)
mean(table1)

t.test()
t = 1.2459, df = 20, p-value = 0.2272
alternative hypothesis: true mean is not equal to 0
95 percent confidence interval:
  -1903.748  7550.414
sample estimates:
  mean of x 
2823.333 

#this shows a rejection to the null hypothesis and favors the alternative hypothesis as it states rthe "true mean is not equal to 0" 
#The results could be different from the whole population because not all ages of kids were studied and the group chosen were higher risk to get covid 
#but also the concern around whether the vaccination was reliable or not couldv'e prevented them getting vaxxed 