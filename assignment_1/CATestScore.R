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

#####################################################################################
# plot some stuff
#####################################################################################

# plot math scores against reading scores
plot(CAS$read,CAS$math,pch=19)

# plot number of students againts number of teachers
plot(CAS$students,CAS$teachers,pch=19)

# plot historgram of student teacher ratio
hist(CAS$str,col="grey")

# plot histogram of test score 
hist(CAS$ts,col="grey")

# plot figure 4.2 in Stock and Watson
plot(CAS$str,CAS$ts,pch=19,xlab="Student-teacher ratio",ylab="Test score")

#####################################################################################
# estimate regression
#####################################################################################

# regress ts on str and store in fm
fm <- lm(ts ~ str, data=CAS)
# print summary
summary(fm)

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
