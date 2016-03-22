require("AER")

#load data and renaming for convenience
load("~/Desktop/econometrics/assignment_4/incomedata.Rda")
incomedata<-id

incomedata$GENED <- (incomedata$GEN*incomedata$ED)
incomedata$LOGEARN <- log(incomedata$EARN)

#Estimating the regression model
fm <- lm(EARN ~ GEN + ED + GENED, data=incomedata)
summary(fm)

#Part d) Wald test
fm0 <- lm(EARN ~ ED, data=incomedata)
fm1 <- lm(EARN ~ GEN + ED + GENED, data=incomedata)
waldtest(fm0,fm1,vcov=vcovHC,test="Chisq")

#Part e) Loginc
fm2 <- lm(LOGEARN ~ GEN + ED + GENED, data=incomedata)
summary(fm2)
