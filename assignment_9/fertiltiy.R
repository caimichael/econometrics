# load packages 
require("graphics")
require("stats")
require("AER")

# import the data file
load("fertility.Rda")

# regress weeks worked on more kids
fm1 <- lm(weeksm1 ~ morekids,data=fertility)
summary(fm1)
coeftest(fm1,vcov=vcovHC(fm1),df=Inf)
