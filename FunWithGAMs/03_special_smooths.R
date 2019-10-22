#Up until now, the examples have just used the default type of smooth, a penalized cubic
# spline.  However, this default smooth is not always appropriate.  Here are a couple of
# examples of more specialized smooths.

#Circular Splines
#Suppose that you want to look at the relationship between time of day, or time of year,
# and your response variable.  If you have data from throughout the entire 24-hour day,
# or from 1 January to 31 December, then you probably want to have your spline predict
# the same value for the start and end of the day/year.  This is possible using 
# a "circular spline".  When you fit a circular spline using mgcv, it assumes that the
# you want to connect up the line at the smallest and largest values for the predictor
# variable, regardless of what these values are.
#You tell "gam" that you want to fit a circular spline by adding an option to the basic
# "s()" statement, in order to change the basis function into a circular cubic spline.
# In the example below, you can see this is done with the option bs = "cc".
#Here is a completely stupid example of how this would work in practice, taking the
# data from the original example in 01_typicalGAMs.R, and forcing the two ends of this
# line to connect.
MyFirstCircularGAM <- gam(cbind(NE_US_prev$NBirdsSick,
                                (NE_US_prev$NBirdsTot - NE_US_prev$NBirdsSick)) ~
                             s(MONTH_FROM_START, bs = "cc") +
                             LATITUDE,
                          offset = NE_US_prev$OffsetValue,
                          family = "binomial",
                          data = NE_US_prev)
#
#Now to see the effect of forcing the two ends of the line to connect, let's compare
# the original spline using the default smooth to the line that is fit when the
# ends of forced to join.
par(mfrow = c(1,2))
plot(MyFirstGAM)
title("Default Smooth")
#
plot(MyFirstCircularGAM)
title("Smooth with Line-ends Joined")
par(mfrow = c(1,1))
#Qualitatively the shapes of the two lines are very similar, especially away from the
# two ends.  However, when the ends are forced to join the probability values are not
# being allowed to be as extreme in order for the two ends to join up.


#Multi-dimensional Splines ("Spline Interactions")
#You can add as many smooth terms to a model as you want.  Each of these smooths is 
# assumed to be independent of the other.  However, sometimes you might be interested
# in knowing how one term of the smooth interacts with a one or more other terms.  For
# a standard GLM this means that you would be interested in modelling a statistical
# interactions.  In GAMland, however, the equivalent is *not* an interaction, but a
# multi-dimentional smooth.  Here's an example of how you can specify a two-dimensional 
# smooth, in which we fit a two-dimensional smooth with MONTH_FROM_START and LATTIUDE
# as the two interacting predictor variables:
MyFirstMultidimensionalGAM <- gam(cbind(NE_US_prev$NBirdsSick,
                                        (NE_US_prev$NBirdsTot - NE_US_prev$NBirdsSick)) ~
                                     s(MONTH_FROM_START, LATITUDE),
                                  offset = NE_US_prev$OffsetValue,
                                  family = "binomial",
                                  data = NE_US_prev)
# Note here that instead of declaring the smooth term by writing
# "s(MONTH_FROM_START, LATITUDE)", which will also work, we have started this
# element of the formula with a "te".  The "te" tells "gam" that we want to fit
# another type of smooth to the data, something called a "tensor spline".  So...
# why?  Tensor splines are appropriate to use when the values of the predictor
# variables are recorded in different units, as is the case here (i.e. here the
# units are days and degrees latitude).  Unless you tell "gam" this important
# information, it will treat the predictor variables as having the same units of
# measure, and as a result the predictor with with values that are numerically 
# larger can potentially overwhelm the small-valued predictor to the point that 
# its effect is treated as being trivial.  However, if the multiple predictors
# are measured in the same units of measure, then a tensor spline is not
# needed and the "s()" specification of the smooth can be used.  If you want to see
# the full list of available types of smooths that can be used within the
# mgcv package you'll find them all in the help files here:
?smooth.terms
#
#Let's look at the output from this two-dimensional smooth.  First, we print out 
# the numeric summary.
summary(MyFirstMultidimensionalGAM)
# What you will notice is that the two-dimensional smooth has a single p-value: you
# cannot say statistically that both of the predictor variables in the smooth
# are important.  Instead, one way of assessing whether it was useful to add LATITUDE
# into the smooth is to compare the AIC values for the model with only
# MONTH_FROM_START in the smooth with the model in which there was a two-dimensional 
# smooth:
AIC(MyFirstGAM)
AIC(MyFirstMultidimensionalGAM)
# The AIC score for the model with the two-dimensional smooth has a lower AIC value,
# and so we can infer that it is a more informative model: the change in disease prevalence
# with time of winter is affected by the latitude at which observations are made.
#
#Now let's look at the default graphical output:
plot(MyFirstMultidimensionalGAM)
# Yeah...that's highly intuitive to interpret (not!).  mgcv's "plot" function will not
# produce a 3-dimensional surface plot.  That's something that you would have to figure
# out how to make yourself.
#Personally, though, I find it easier to visualize the meaning of a 2- (or more-)dimensional
# surface by decomposing it into simpler, single-dimension graphs: looking at the effect of
# one of the predictor variables whilst holding the other(s) constant.  This requires 
# using mgcv's "predict" function in order to calculate predicted values for a pre-specified
# set of combinations of values for the predictor variables.  The mgcv "predict" function
# works basically like any other GLM-type predict function in R.  Here is an example of how to
# create a set of fake data and then calculate predicted values that can subsequently be
# graphed.
#We'll start by gradually building up a table with the two predictor variables that are named in
# the original model.  We will be asking for one prediction to be made within the full range of
# months within the data, and for two arbitrarily-chosen latitudes.  We will create a separate
# set of fake data for each of these two latitudes, although these could be combined into a 
# single 2-column table.
MonthVector <- seq(from = min(NE_US_prev$MONTH_FROM_START), to = max(NE_US_prev$MONTH_FROM_START))
ConstantLat42 <- rep(x = 42, times = length(MonthVector))
ConstantLat40 <- rep(x = 40, times = length(MonthVector))
FakeData_Lat42 <- tibble(MONTH_FROM_START = MonthVector, LATITUDE = ConstantLat42)
FakeData_Lat40 <- tibble(MONTH_FROM_START = MonthVector, LATITUDE = ConstantLat40)
#Now that we have our data, let's calculate some predicted values

FakeData_Lat42 <- cbind(FakeData_Lat42,
                        PredProb = predict(MyFirstMultidimensionalGAM, newdata = FakeData_Lat42, type = "response"))
FakeData_Lat40 <- cbind(FakeData_Lat40,
                        PredProb = predict(MyFirstMultidimensionalGAM, newdata = FakeData_Lat40, type = "response"))
#Now we can visualize how the probability of disease has been modelled to vary at these two different
# latitudes, and across the entire time-period of the study.
plot(range(MonthVector), c(0, max(c(FakeData_Lat42$PredProb, FakeData_Lat40$PredProb))), 
     type = "n", 
     xlab = "Months From Start of Study",
     ylab = "Predicted Disease Prevalence")
lines(FakeData_Lat40$MONTH_FROM_START, FakeData_Lat40$PredProb, lwd = 3, col = "red")
lines(FakeData_Lat42$MONTH_FROM_START, FakeData_Lat42$PredProb, lwd = 3, col = "blue")
#You'll note that the shapes of the two lines, while similar, are not the same: the surface
# does vary in a manner that indicates that the date and latitude are interacting to 
# predict disease prevalence.


#GAM Options Galore!
#The above examples provide some indication of the diversity of options available within the 
# "mgcv" package.  Likely these would suffice for most purposes.  However, there are other
# packages in R that offer an even greater diversity of ways in which splines can be fit
# through data.  Here are a selection of other possibilities.
#
#Mixed Models: While "mgcv" does allow for random effects to be used in the models that it
# fits, the handling of random effects is done better in an adaptation of mgcv that is built
# on top of "lme4".  This alternative package, "gamm4", was also created by Simon Wood, the 
# creator of "mgcv". Assuming that the library is installed on your computer, you can read
# the documentation here:
library(gamm4)
?gamm4
#
#Splines Meet Quantile Regression: Two of my favourite, esoteric GLMs rolled into one!  The 
# "gam" part of "qgam" should be self explanatory.  Quantile regression is a form of GLMish
# in which the user specifies which part of the distribution of errors is fit by the line.
# For example, one can fit a regression line through the median (roughly equivalent to the
# mean values fit by a standard GLM), but also through other quantiles like the 5% or 95% 
# values, or anything in between.  Again, assuming that the appropriate library is residing
# on your computer, you can read more here:
library(qgam)
?qgam
#
#Fitting Changes in Mean and Variance Simultaneously: So, the standard GLM assumes that the
# the distribution of errors is constant across the entire range of values for the predictor
# variables.  That's statistically convenient...but not necessarily a good match to
# reality.  That's where gamlss models (Generalized Additive Models for Location, Scale and
# Shape) are useful.  It is possible to do things like simultaneously model changes in mean
# and error variance as splines.  If the library is installed on your computer, you can read
# the basic documentation here:
library(gamlss)
?gamlss