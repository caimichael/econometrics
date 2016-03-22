# birthweight smoking exercise for problem set
# make sure you save the file "birthweight_smoking.csv" to the working directory

# load packages -- you may need to instal them first; click the packages tab
require("graphics")
require("stats")
require("sandwich")
require("lmtest")
require("car")

# import the data
dat <- read.csv("birthweight_smoking.csv",header=TRUE)

# first regression
fm0 <- lm(birthweight ~ smoker,data=dat)
summary(fm0)
coeftest(fm0,df=Inf,vcov=vcovHC(fm0))
