#Loading DATA



#Cleaning DATA: one value was "kayık" fış fış kürek
algerian_forest$Classes[166] <- "fire"
algerian_forest$FWI[166] <- NA
algerian_forest$Classes <- trimws(algerian_forest[,"Classes"])

#New Column fire_status
algerian_forest$fire_status <- ifelse(algerian_forest[,"Classes"]=="not fire",0,1)

#Factorizing
algerian_forest$Classes <- as.factor(algerian_forest$Classes)

algerian_forest$DC[166] <- 14.69

algerian_forest$DC1 <- as.numeric(algerian_forest$DC)

algerian_forest$FWI1 <- as.numeric(algerian_forest$FWI)

algerian_forest <- algerian_forest[,-c(1,2,3,7)]



#Rain data is too small to make any comment on it
subset(portugal_forest_fire, select = -rain)

#Determine whether there was a fire or not
portugal_forest_fire$fire_status <- ifelse(portugal_forest_fire[,"area"]>0,1,0)

portugal_forest_fire <- portugal_forest_fire[,-c(1,2,3,4,12)]


analysis_vector <- c("This analyse is made with linear regression and logistic regression. 
As you can see in the summary part of lm, output of the p-value (intercept) is significantly small, so Ho can be rejected. However, adjusted Rsquared is too small. Hence, the model is not a good fit for the data.
In the summary of glm, the difference between Null deviance and Residual deviance is too small, the model is not a good fit for the data.","This analyse is made with logistic regression. 

In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.

When beta1 coefficient is exponentiated, the output is 1.47143. Hence, we can say that the association between Fire Status and Temperature is positive.

Odds of fire breaking out increases by 1.47143 for each one unit (degree) change in temperature.","This analyse is made with logistic regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, the model is good for the data.
When beta1 coefficient is exponentiated, the output is 0.9308206 Hence, we can say that the association between Fire Status and RH is negative.
Odds of fire breaking out changes by 0.9308206 for each one unit (degree) change in temperature.","This analyse is made with logistic regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.
When beta1 coefficient is exponentiated, the output is 3591.647  . Hence, we can say that the association between Fire Status and Temperature is positive.
Odds of fire breaking out increases by 3591.647   for each one unit (degree) change in temperature.","This analyse is made with logistic regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.
When beta1 coefficient is exponentiated, the output is 5.534967 . Hence, we can say that the association between Fire Status and Temperature is positive.
Odds of fire breaking out increases by 5.534967  for each one unit (degree) change in temperature.","This analyse is made with logistic regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.
When beta1 coefficient is exponentiated, the output is 1.323033  . Hence, we can say that the association between Fire Status and Temperature is positive.
Odds of fire breaking out increases by 1.323033  for each one unit (degree) change in temperature.","This analyse is made with linear regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. However, adjusted Rsquared is small, the model is not a good fit for the data.","This analyse is made with logistic regression. 
In the summary, output of the p-value (intercept) is significantly small, so Ho can be rejected. Moreover, the difference between Null deviance and Residual deviance is high enough, that is to say the model is good for the data.
When beta1 coefficient is exponentiated, the output is 1.2673   . Hence, we can say that the association between Fire Status and Temperature is positive.
Odds of fire breaking out increases by 1.2673    for each one unit (degree) change in temperature.")
