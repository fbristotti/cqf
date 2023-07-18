######################################################################
# 2018 Revision. Richard Diamond. Queries to r.diamond@cqf.com       #
# Models are specified and validated but any use is at your own risk #
######################################################################

# ERROR CORRECTION MODEL (Engle-Granger procedure, two variables)

# ADF TEST ON PRICE SERIES
# "drift" refers Delta Y= ... + constant
# "trend" refers Delta Y = ... + beta*t  # comes up significant but overfits time dependence
adf.test = ur.df(curve2.this$X10, type = "drift")
print(summary(adf.test))

adf.test = ur.df(curve2.this$X25, type = "drift")
print(summary(adf.test))

# NAIVE COINTEGRATING EQUATION

coint.reg = lm(curve2.this$X10 ~ curve2.this$X25)
print(summary(coint.reg))

# CADF TEST ON RESIDUAL

cadf.test = ur.df(residuals(coint.reg), type = "none") # CADF because ADF test applies to cointegrated residual
print(summary(cadf.test))

# EC EQUATION STIMATION (10Y on 25Y)

tenorY.diff = diff(curve2.this$X10) 
tenorX.diff = diff(curve2.this$X25)

ec_term.lag = lag(residuals(coint.reg), k = -1)
ecm.reg = lm(tenorY.diff ~ tenorX.diff + ec_term.lag + 0) # we want to avoid the extra constant outside of ec_term.lag
print(summary(ecm.reg))

#EC with an augment Delta Y_t-1 # HAVE TO make time series of equal length and drop the earliest historical observation
ecm_aug.reg = lm(tenorY.diff[ time(tenorY.diff) != as.Date("2013-05-31")] ~ lag(tenorY.diff, k = -1) + tenorX.diff[ time(tenorX.diff) != as.Date("2013-05-31")] + ec_term.lag[ time(ec_term.lag) != as.Date("2013-05-31")] + 0)
print(summary(ecm_aug.reg)) # not significant by t statistic


# EC EQUATION STIMATION (25Y on 10Y) -- REQUIRED we recalculate coint. relationship for this lead-lag relationship (causality form)
coint1.reg = lm(curve2.this$X25 ~ curve2.this$X10)
print(summary(coint1.reg))

ec_term1.lag = lag(residuals(coint1.reg), k = -1)
ecm1.reg = lm(tenorX.diff ~ tenorY.diff + ec_term1.lag + 0)
print(summary(ecm1.reg))



# LINEAR REGRESSION ON DIFFERENCES // linear regression in differences gives certain hedge ratio (the minimum variance-like) 

simple.reg = lm(diff(curve2.this$X10) ~ diff(curve2.this$X25) + 0) # + 0 means no cash holdings
print(summary(simple.reg))
