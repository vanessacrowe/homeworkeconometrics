summary(Household_Pulse_data$KIDGETVAC_LT5Y)
#Vanessa Crowe
#remove NA
dat_kidvaxx_nonmissing <- subset(Household_Pulse_data, (Household_Pulse_data$KIDGETVAC_LT5Y != "NA") )
summary(dat_kidvaxx_nonmissing)
#We’ll transform “definitely yes” to be 5, “probably yes” to be 4, “probably no” to 2, and “definitely no” to 1. 
#Then both “unsure” and “do not know plans” are coded as 3.

#give value to each answer for easier comparison
#chose less than 5 because people may be more careful with younger kids so the data will skew towards them being vaxxed
temp1 <- fct_recode(dat_kidvaxx_nonmissing$KIDGETVAC_LT5Y, '5' = 'kids under 5yo definitely get vaxx',
                    '4'='kids under 5yo probably get vaxx', '3'='unsure kids under 5yo get vaxx',
                    '2'='kids under 5yo probably NOT get vaxx', '1'='kids under 5yo definitely NOT get vaxx',
                    '3'='do not know plans for vaxx for kids under 5')
#so far, this match with my prediction of a significantly higher amount of kids less than 5yo getting vaxxed
summary(temp1)
# this converts factor to numeric, note the odd syntax
kidsvaxLT5 <- as.numeric(levels(temp1))[temp1]
summary(kidsvaxLT5)

#normalize numeric vector(X_in) to a range between 0 and 1, min-max scaling
#0 corresponds to the minimum value while 1 is the max 
norm_varb <- function(X_in) {
  (X_in - min(X_in, na.rm = TRUE))/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE)  )
}

norm_varb
# this is a lazier way ot converting factors to numbers, uses the order of the levels
#normalizing the variables and placing them between 0 and 1

#measuring the relationship between kids getting vaxxed vs education level and race 
#the responses to education level and race were the simplest  and I thought higher education
#level would make a parent more likely to vaccinate their child 
#I wanted to view race because the vaccine was heavily politicized and there were many reports of 
#the disparity between white, black, asian, and hispanic people hesitancy about the vaccine
#predicting that hispanic individuals would be less likely since it was reported by the CDC they 
#had low percentages of 1st and 2nd doses compared to their counterparts
data_use_prelim <- data.frame(norm_varb(as.numeric(dat_kidvaxx_nonmissing$EEDUC)),norm_varb(as.numeric(dat_kidvaxx_nonmissing$RRACE)))
data_use_prelim

good_obs_data_use <- complete.cases(data_use_prelim,dat_kidvaxx_nonmissing$KIDGETVAC_LT5Y)
dat_use <- subset(data_use_prelim,good_obs_data_use)
y_use <- subset(kidsvaxLT5,good_obs_data_use)

set.seed(12345)
NN_obs <- sum(good_obs_data_use == 1)
select1 <- (runif(NN_obs) < 0.8)
train_data <- subset(dat_use,select1)
test_data <- subset(dat_use,(!select1))
cl_data <- y_use[select1]
true_data <- y_use[!select1]
#computing closeness knn is similar to ranking

summary(cl_data)
summary(train_data)

#loop with sequence creating 1,3,5,7 values in each iteration
#computing the closeness with knn(k-nearest numbers)
#calculates the correct predictions, predicted vs true
#
for (indx in seq(1, 9, by= 2)) {
  pred_y <- knn3Train(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
  num_correct_labels <- sum(pred_y == true_data)
  correct_rate <- num_correct_labels/length(true_data)
  print(c(indx,correct_rate))
}

cl_data_n <- as.numeric(cl_data)
summary(as.factor(cl_data_n))
names(train_data) <- c("norm_educ","norm_race")


model_ols1 <- lm(cl_data_n ~ train_data$norm_educ + train_data$norm_race)

y_hat <- fitted.values(model_ols1)

mean(y_hat[cl_data_n == 2])
mean(y_hat[cl_data_n == 3])
mean(y_hat[cl_data_n == 4])
mean(y_hat[cl_data_n == 5])

# maybe try classifying one at a time with OLS

cl_data_n2 <- as.numeric(cl_data_n == 2) 
# now this is only 1 or 0, depending whether the condition is true or false

model_ols_v2 <- lm(cl_data_n2 ~ train_data$norm_educ + train_data$norm_race)
y_hat_v2 <- fitted.values(model_ols_v2)
mean(y_hat_v2[cl_data_n2 == 1])
mean(y_hat_v2[cl_data_n2 == 0])

# can you do better?

sum(is.na(Household_Pulse_data$KIDGETVAC_LT5Y))

sum(as.numeric(Household_Pulse_data$KIDGETVAC_LT5Y == "NA"))

model_ols1
summary(model_ols1)
model_ols_v2
summary(model_ols_v2)

#depending on the education level/the parents' race, kids getting vaxxed will increased or decrease
#v1
#correlation between kids getting vaxxed and education level
#The coefficient of .6839 suggests that every time the education level increases, the amount of kids
#getting vaxxed also does
#the SD of 0.08546 suggests that the result of the coefficient is precise due to how small it is
#In this case, the t value being 8.003 suggests that the relationship is statistically significant
#because it is far away from zero

#this specific data was interesting because when looking at the stats for model_ols_v2, the data did
#not read as statistically significant with coefficient of .04573, SD of .02492 t value of 1.835 and
#the p value was .0665 which is very close to the threshold of .05
#This indicates there is a relationship between the variables but may not be significant based on the p value

#v1
#correlation between kids getting vaxxed and race
#on average there is a .4121 increase of kids getting vaccinated based on change in "race"
#The t value (5.487) and low p value of (4.38e-08) suggests this data is statistically significant


