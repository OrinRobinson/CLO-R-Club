#Load libraries.
library(tidyverse)  #Used for data manipulation
library(mgcv)       #Used for fitting GAMs

#Load some data, with which we can play.  Then have a quick look at
# the structure of these data (which are records of observed incidence
# of conjunctivitis disease in House Finches in the northeastern U.S.
# over a series of years).
load(file = "data/NE_US_MGprev_31July2016.RData")
str(NE_US_prev)

#Fit a basic GAM to the long-term variation in prevalence of
# disease.  Time is measured as months from the first data
# to be collected.
#The formula used to fit a GAM is very similar to that used for a standard
# GLM.  The main difference is that predictor variables can be specified
# to be "smooth" terms in the model, done here by enclosing the name of the
# predictor within parentheses, and putting an "s" (for smooth) in front.
#Note also that just as with GLMs in R, different families of error distribution
# can be specified, and in this case we are running a logistic regression (i.e. a
# regression with a binomial error term).  Also, we are specifying the response
# as an odds ratio by feeding in a matrix with 2 columns for each row: the
# number of diseased birds, and the number of non-diseased birds...just as can be
# done with GLMs.  There's also an offset specified, which in this case is a
# calibration value that accounts for differences in reported prevalence of disease
# for data that come from different sources.
MyFirstGAM <- gam(cbind(NE_US_prev$NBirdsSick,
                        (NE_US_prev$NBirdsTot - NE_US_prev$NBirdsSick)) ~
                     s(MONTH_FROM_START) +
                     LATITUDE,
                   offset = NE_US_prev$OffsetValue,
                   family = "binomial",
                   data = NE_US_prev)
#
#The summary of GAM output should look very similar to that from a GLM, except that
# there is a separation between the output from athe non-smooth terms and the smooth term.
#Note that there is no regression coefficient or standard error provided for the
# smooth term, because there is no single slope.  An approximate p-value is provided.
summary(MyFirstGAM)
#
#The output from using a "plot" function is also a bit different than what one would
# get from a GLM: there are no diagnostics plotted, just a graph displaying the form 
# of the smooth effect.  Note that the y-axis of the graph is an arbitrary range of
# values that are centered on zero, which represents the average value of the effect
# of the smooth term.
#You can see in the plot that there is a very arbitrary-looking wiggly line that describes
# variation in the observed prevalence of disease, and the 95% confidence limits around this
# like probide a visual description of when changes are predicted to be statistically
# substantial.  With over 55,000 data points, it's not surprising that the confidence interval
# is rather narrow.
plot(MyFirstGAM)

#While the default plot provides useful information, it doesn't provide information about the
# magnitude of change in biologically informative units.  In order to create such a plot,
# we need to make use of the "predict" function associated with mgcv output objects.  Below
# we create a set of fake data on which predictions are made.  Predictions can be produced either
# on the link scale (as log-odds of reporting of a diseased bird) for a logistic regression model), 
# or the response scale (probability of disease).  In order to calculate approximate 95% confidence
# intervals we will need to have "predict" calculate predicted values and their standard errors
# on the logit scale, then calculate approximate 95% confidence intervals, before back-transforming
# these values into probabilities ourselves.
FakeData <- data.frame(MONTH_FROM_START = seq(from = min(NE_US_prev$MONTH_FROM_START),
                                      to = max(NE_US_prev$MONTH_FROM_START)),
                       LATITUDE = 42)
PredictOutput <- predict(MyFirstGAM, newdata = FakeData, se.fit = T)
PredictedPrevalence <- data.frame(ProbDisease = exp(PredictOutput[[1]]/(1 + exp(PredictOutput[[1]]))),
                                  LowerCL = exp((PredictOutput[[1]] - 2*PredictOutput[[2]])/
                                                   (1 + exp(PredictOutput[[1]] - 2*PredictOutput[[2]]))),
                                  UpperCL = exp((PredictOutput[[1]] + 2*PredictOutput[[2]])/
                                                   (1 + exp(PredictOutput[[1]] + 2*PredictOutput[[2]]))))
plot(range(FakeData), c(0, 0.075), 
     xlab = "Months from Start of Study", 
     ylab = "Per Bird Probability of Disease", 
     type = "n")
lines(FakeData$MONTH_FROM_START, PredictedPrevalence$ProbDisease, lwd = 2, col = "red")
lines(FakeData$MONTH_FROM_START, PredictedPrevalence$LowerCL, lwd = 1, lty = 3, col = "black")
lines(FakeData$MONTH_FROM_START, PredictedPrevalence$UpperCL, lwd = 1, lty = 3, col = "black")
points(x = NE_US_prev$MONTH_FROM_START,  
       y = jitter(NE_US_prev$NBirdsSick/NE_US_prev$NBirdsTot),
       pch = 18,
       cex = 0.5,
       col = "gray")