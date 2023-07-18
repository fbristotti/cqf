######################################################################
# 2018 Revision. Richard Diamond. Queries to r.diamond@cqf.com       #
# Models are specified and validated but any use is at your own risk #
######################################################################

# Install packages (once)
# install.packages("quantmod") # Includes object types xts, TTR and functions for time series analsysis.
# install.packages("urca") # Includes the ur.df() function for the Dickey-Fuller Unit Root test.

# Load required library
library(quantmod)
library(zoo)
library(urca)

# Delete any existing data
rm(list = ls(all.names = TRUE))
setwd(utils::getSrcDirectory(function(){}))

# COINTEGRATION IN SPOT RATES (with AUTOCORRELATION PROFILE) // # COINTEGRATION IN FORWARD RATES (with AUTOCORRELATION PROFILE)

curve.zoo = read.zoo("spot_curve.csv", header=TRUE, sep=",", format = "%d/%m/%y") # %y Year (2 digit) // %Y Year (4 digit)
curve.zoo = curve.zoo[complete.cases(curve.zoo),] # remove empty lines, also  curve.zoo = curve.zoo[rowSums(is.na(curve.zoo)) == 0,]

# SELECT SUBSAMPLES .this
curve.this = window(curve.zoo, start=as.Date("2013-5-30"), end=as.Date("2015-5-30")) 
# curve.this = curve.zoo[as.Date(c("2015-1-1", "2015-5-30"))] #This alternative method not working, needs correction in index


# SCREENING WITH MULTIVARIATE COINTEGRATION ANALYSIS - JOHANSEN PROCEDURE

#RD: Running Johansen Test for the full matrix up to 25Y tenor. Result: approx r<=40 cointegrated pairs!!
#RD: Let's pick up tenors 0.08, 25. Result: no Johansen cointegration
#RD: As we include more tenors, one or more cointegrating relationship transpires

curve2.this = curve.this[, colnames(curve.this) %in% c("X0.08","X1", "X3", "X6", "X10", "X20", "X25")] 

curve2.this = curve.this[, colnames(curve.this) %in% c("X0.08","X1","X2")] # cointegrated all data // not cointegrated 2013-05 to 2015-05 ***

curve2.this = curve.this[, colnames(curve.this) %in% c("X0.08","X1")] # cointegrated all data // not cointegrated 2013-05 to 2015-05 ?
curve2.this = curve.this[, colnames(curve.this) %in% c("X1","X2")] # not cointegrated all data // not cointegrated 2013-05 to 2015-05 ? 

curve2.this = curve.this[, colnames(curve.this) %in% c("X10", "X20", "X25")] # not cointegrated all data // cointegrated 2013-05 to 2015-05 ***

curve2.this = curve.this[, colnames(curve.this) %in% c("X10", "X20")] # N/A // cointegrated 2013-05 to 2015-05
curve2.this = curve.this[, colnames(curve.this) %in% c("X10", "X25")] # N/A // cointegrated 2013-05 to 2015-05 MODEL CHOICE for important tenors

curve2.this = curve.this[, colnames(curve.this) %in% c("X20", "X25")] # N/A // near-cointegrated 2013-05 to 2015-05


# JOHANSEN PROCEDURE ( this is where the work happens, inside ca.jo() )

johansen.test = ca.jo(curve2.this, ecdet = "const", type="eigen", K=2, spec="longrun")
cajorls(johansen.test) # OLS regression of restricted VECM -- EC-term (long run) and past differences (short run)
#cajools(johansen.test) # OLS regression of unrestricted VECM -- here of little value because it mixes past differences and past levels


# HOW MANY PAST VALUES (LAGS) TO INCLUDE?

# The loop below iterates from lag=2 to lag=5. One formally confirms lag by computing AIC/BIC for the appropriate VECM. 
# However for a data analysis, it is useful to trace if coint appears/strenthens/weakens depending on lags included
for(k in seq(2,5)) {
  
  print("######################################")
  print(paste("#              Lag = ",k,"               #",sep=""))
  print("######################################")
  
  # Run Johansen Maximum Eigen Statistic Test on prices with trend and lag of k
  print(summary(ca.jo(curve2.this, type="eigen", ecdet="trend", K=k)))
  
  # Run Johansen Trace Statistic Test on prices with trend and lag of k
  print(summary(ca.jo(curve2.this, type="trace", ecdet="trend", K=k)))

}



######################################################################
# 2015. Richard Diamond. Quieries to r.diamond@cqf.com               #
# CQF Cointegration Lecture: Explorations                            #
######################################################################

# Install packages ONCE
# install.packages("sde") # Includes object types xts, TTR and functions for time series analsysis.
# Load required package
library(sde)

# Simulate two Brownian Motions and conduct simple regression on them
BM1.sim = BM(x=0, t0=0, T=5, N=(5*252*8))
BM2.sim = BM(x=0, t0=0, T=5, N=(5*252*8))

# Confirm the unit root (non-stationarity) in Brownian Motion as an integrated process 
adf.test = ur.df(BM1.sim, type = "none")
print(summary(adf.test))

# SPURIOUS REGRESSION: significant t statistic but those are two ORTHOGONAL PROCESSES (in differences)
linear.reg = lm(BM1.sim ~ BM2.sim + 1) 
print(summary(linear.reg))

plot(BM1.sim, plot.type="single", main = "Simulated Brownian Motion", xlab="Time")
plot(BM2.sim, BM1.sim, plot.type="single", main = "Two-Dimensional Brownian Motion") # Dimensions MUST BE orthogonal, not plotted aganst one another


# BIMODAL HISTOGRAM -- any nonstatinary time series, including BM, produces it 
hist(BM1.sim, breaks=100, main = "(Bi-modal) Histogram of Non-stationary Process", xlab="%") #Set freq=FALSE for a density plot
# hist(curve2.this, breaks=10, main = "Histogram of sport rate (10Y) in levels", xlab="%") #Set freq=FALSE for a density plot
