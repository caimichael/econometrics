# Estimates capital asset pring model using OLS
 
# you will need to download the file "capm.dta" and set your working directory to the folder in which the file is stored
# use the command "setwd"
 
# load the data
load("capm.dta")
 
# regress excess return on apple (rapl-rf) on the excess return on the market (rmkt-rf)
fm <- lm( (rapl-rf) ~  (rmkt-rf),data=capm)
summary(fm)
 
# scatter plot with line of best fit
plot( (capm$rmkt-capm$rf),(capm$rapl-capm$rf),pch=19 )
abline(fm)

# regress excess return on verizon (rvzc-rf) on the excess return on the market (rmkt-rf)
fm <- lm( (rvzc-rf) ~  (rmkt-rf),data=capm)
summary(fm)

# scatter plot with line of best fit
plot( (capm$rmkt-capm$rf),(capm$rvzc-capm$rf),pch=19 )
abline(fm)

