#There are at least two traits of GAMs that require caution during interpretation.  In this
# script I will illustrate these.

#CAUTION #1: "mgcv" selects the number and locations of the knot points automatically.
# Mostly this is a really good thing.  By default knot points are spaced evenly across
# the range of predictor variables which is probably reasonable for most purposes.
# However, the selection of the number of knot points is not always biologically rational
# in my experience: the selection process is designed to be *statistically* reasonable.
# However, the specific definition of "statistically-reasonable" that is used will
# depend on sample sizes: the bigger the sample size, the greater the number of knot
# points that are allowed, and the wigglier the GAMs smooth curve can become.
#CAUTION #2: Wiggliness of lines also depends on the relative 

DecimationData <- sample_n(tbl = NE_US_prev,
                           size = nrow(NE_US_prev)/10,
                           replace = FALSE)


par(mfcol = c(1,2))
par(mar = c(4.1, 4.1, 4.1, 1.1))
plot(gam(cbind(NE_US_prev$NBirdsSick,
               (NE_US_prev$NBirdsTot - NE_US_prev$NBirdsSick)) ~
            s(MONTH_FROM_START) + LATITUDE,
         offset = NE_US_prev$OffsetValue,
         family = "binomial",
         data = NE_US_prev))
title("GAM Using All Data")
plot(gam(cbind(DecimationData$NBirdsSick,
               (DecimationData$NBirdsTot - DecimationData$NBirdsSick)) ~
            s(MONTH_FROM_START),
         offset = DecimationData$OffsetValue,
         family = "binomial",
         data = DecimationData))
title("GAM Using 10% of Data")
par(mfcol = c(1,1))
#
#If the amount of wiggliness looks to high it can (should?) be adjusted to taste.
# This can be done by specifying the maximum number of knot points that will be
# used (in mgcv the actual number cannot necessarily be specified, if that number
# is higher than the number that the "gam" function things is high enough).
#Below, we fit two additional models to add to the two that we have just created,
# telling "gam" the maximum number of knots.
par(mfrow = c(2,2))
plot(gam(cbind(NE_US_prev$NBirdsSick,
               (NE_US_prev$NBirdsTot - NE_US_prev$NBirdsSick)) ~
            s(MONTH_FROM_START) + LATITUDE,
         offset = NE_US_prev$OffsetValue,
         family = "binomial",
         data = NE_US_prev))
title("GAM Using All Data")
plot(gam(cbind(DecimationData$NBirdsSick,
               (DecimationData$NBirdsTot - DecimationData$NBirdsSick)) ~
            s(MONTH_FROM_START) + LATITUDE,
         offset = DecimationData$OffsetValue,
         family = "binomial",
         data = DecimationData))
title("GAM Using 10% of Data, Automatic Knot Selection")
plot(gam(cbind(DecimationData$NBirdsSick,
               (DecimationData$NBirdsTot - DecimationData$NBirdsSick)) ~
            s(MONTH_FROM_START, k = 4) + LATITUDE,
         offset = DecimationData$OffsetValue,
         family = "binomial",
         data = DecimationData))
title("GAM Using 10% of Data, 4 Knot Points")
plot(gam(cbind(DecimationData$NBirdsSick,
               (DecimationData$NBirdsTot - DecimationData$NBirdsSick)) ~
            s(MONTH_FROM_START, k = 20) + LATITUDE,
         offset = DecimationData$OffsetValue,
         family = "binomial",
         data = DecimationData))
title("GAM Using 10% of Data, 20 Knot Points")
par(mfcol = c(1,1))

#CAUTION #2: Never, ever extrapolate beyond the limits of data.  Actually, for that
# matter, it's best not to even trust the ends of smooths, especially if there are
# knot points close to the ends that would allow a small number of extreme data points
# to pull the ends of the curves in some extreme directly.
#One can see the problems with extrapolation in this example below, which takes the
# original GAM and estimates predicted values far beyond the limits of the actual
# time period.
FakeData2 <- data.frame(MONTH_FROM_START = seq(from = 1, to = 300),
                        LATITUDE = 42)
ExtrapolatedPrevalence <- predict(MyFirstGAM,
                                  newdata = FakeData2,
                                  se.fit = FALSE,
                                  type = "response")
plot(range(FakeData2), c(0, 0.075), 
     xlab = "Months from Start of Study", 
     ylab = "Per Bird Probability of Disease", 
     type = "n")
lines(FakeData2$MONTH_FROM_START, ExtrapolatedPrevalence, lwd = 3, col = "black")
WhichIsNotExtrapolation <- which(FakeData2$MONTH_FROM_START %in% FakeData$MONTH_FROM_START)
lines(x = FakeData2$MONTH_FROM_START[WhichIsNotExtrapolation], 
      y = ExtrapolatedPrevalence[WhichIsNotExtrapolation], 
      lwd = 4, 
      col = "red")
title("Extrapolations (in black) Just Keep Going...")