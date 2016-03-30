#####################################################################################
# California school data
#####################################################################################

# load packages 
require("graphics")
require("stats")
require("AER")

# load data and rename
data("CASchools")
CAS <- CASchools
rm(CASchools)

# create test score and student teacher ratio variables
# average of math and reading score -- what SW call ''Test Score''
CAS$ts <- (CAS$math+CAS$read)/2  
# student teacher ratio
CAS$str <- CAS$students/CAS$teachers 
CAS$strsq <- (CAS$str)^2
CAS$strcu <- (CAS$str)^3

CAS$incomeclasssize <- CAS$income*CAS$str
CAS$sizedummy <- 

#Estimate models
fm1 = lm(ts ~ str+strsq+strcu+lunch+log(income), data=CAS)
fm2 = lm(ts ~ str+strsq+strcu+lunch+expenditure+income, data=CAS)
fm3 = lm(ts~ str+strsq+strcu+lunch+log(incomeclasssize), data=CAS)


#Show Summaries
summary(fm1)
coeftest(fm1,df=Inf,vcov=vcovHC(fm1))
summary(fm2)
coeftest(fm2,df=Inf,vcov=vcovHC(fm2))

summary(fm3)
coeftest(fm3,df=Inf,vcov=vcovHC(fm3))

AIC(fm1,fm2,fm3)
BIC(fm1,fm2,fm3)
