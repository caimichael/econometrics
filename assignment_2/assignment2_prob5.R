#####################################################################################
# California school data
#####################################################################################

#####################################################################################
# load packages -- you may need to instal them first; click the packages tab
#####################################################################################

require("graphics")
require("stats")
require("AER")

#####################################################################################
# load data and rename
#####################################################################################

data("CASchools")
CAS <- CASchools

#####################################################################################
# create test score and student teacher ratio variables
#####################################################################################

# average of math and reading score -- what SW call ''Test Score''
CAS$ts <- (CAS$math+CAS$read)/2  

# student teacher ratio
CAS$str <- CAS$students/CAS$teachers 

# Increasing expenditure by 1000
CAS$incexpenditure <- 1000 + CAS$expenditure

#####################################################################################
# estimate regression
#####################################################################################

# regress ts on str and store in fm
fm1 <- lm(ts ~ expenditure, data=CAS)
fm2 <- lm(ts ~ incexpenditure, data=CAS)
# print summary
summary(fm1)
summary(fm2)

# add line of best fit to scatter plot
abline(fm)

#####################################################################################
# estimate regression by brute force
#####################################################################################

# beta1_hat
beta1_hat <- cov(CAS$str,CAS$ts)/(var(CAS$str))

# beta0_hat
beta0_hat <- mean(CAS$ts) - beta1_hat*mean(CAS$str)

# print the output
cat("OLS estimates: beta0 = ",beta0_hat," beta1 = ",beta1_hat,"\n")

#####################################################################################
# plot residuals against regressor
#####################################################################################

plot(CAS$str,fm$residuals,pch=19)
abline(0,0)

# mean of residuals
mean(fm$residuals)

# mean of residuals times regressor
mean(fm$residuals*CAS$str)
