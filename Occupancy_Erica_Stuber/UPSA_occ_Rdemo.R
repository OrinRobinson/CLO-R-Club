setwd("C:/Users/meep/Desktop/")

#-------------------------------------------------------------


# Occupancy Modeling

#------------------------------------------


# load data
upsa_occ <- read.csv(file="upsa_occ_demo.csv", sep=",", header=TRUE)
str(upsa_occ) # examine the data

# load library
#install.packages("unmarked")
library(unmarked)

#### Set up data ####

# isolate count data
upsa_occ.y <- as.matrix(upsa_occ[,c("y.1", "y.2", "y.3")])
upsa_occ.y[1:5,]

# load site covariates (landcover data from remote sensing product), site identity, and year
upsa_occ.site <- data.frame(upsa_occ[,5:11])
upsa_occ.site[1:5,]

# remove year effect from julian date
upsa_occ$jdate.1<- upsa_occ$jdate.1%%1000
upsa_occ$jdate.2<- upsa_occ$jdate.2%%1000
upsa_occ$jdate.3<- upsa_occ$jdate.3%%1000

# Survey covaraites
jdate <- as.matrix(upsa_occ[,c("jdate.1", "jdate.2", "jdate.3")])
minutes <- as.matrix(upsa_occ[,c("minutes.1", "minutes.2", "minutes.3")])
temp <- as.matrix(upsa_occ[,c("temp.1", "temp.2", "temp.3")])
observ <- as.matrix(upsa_occ[,c("obs.1", "obs.2", "obs.3")])
upsa_occ.obs <- list(jdate=jdate, minutes=minutes,temp=temp, observ=observ)
upsa_occ.obs$jdate[1:5,]
upsa_occ.obs$temp[1:5,]
upsa_occ.obs$observ[1:5,]

# Create an unmarked frame from the data
upsa_occUMF <- unmarkedFrameOccu(y = upsa_occ.y, siteCovs = upsa_occ.site, obsCovs = upsa_occ.obs)

# Extract site covariates as a data frame
sc <- siteCovs(upsa_occUMF)
names(sc)

# Create new variables for polynomial functions
sc$allgrss100sq <- (sc$allgrss100 * sc$allgrss100)
sc$trees100sq <- (sc$trees100 * sc$trees100)

sc$year <- as.factor(sc$year)
# Scale (standardize) site covariates where x-mean/sd
sc[,6:9] <- scale(sc[,6:9])

head(sc)

# Return covariates to original dataframe
siteCovs(upsa_occUMF) <- sc

# Create new variables for observer covariates and standardize
oc <- obsCovs(upsa_occUMF)

# Create a new quadratic variable for minutes
oc$minutessq <- (oc$minutes * oc$minutes)

# Scale observation covariates
oc$jdate <- scale(oc$jdate)
oc$minutes <- scale(oc$minutes)
oc$minutessq <- scale(oc$minutessq)

# examine survey specific covariates
str(oc)

# Return observer covariates to unmarked frame
obsCovs(upsa_occUMF) <- oc


#-----------------------------------------------------------

#### Run unmarked occupancy model ####


# Model code occu(~"detection" ~ "occurrence", data, mixture = ("Poisson"), etc.
upsa_occ.1 <- occu(~ jdate + minutes + minutessq ~ allgrss100 + allgrss100sq +  trees100 + trees100sq + year -1, data=upsa_occUMF, se=TRUE)
upsa_occ.2 <- occu(~ jdate + minutes + minutessq ~ year -1, data=upsa_occUMF, se=TRUE)
upsa_occ.int <- occu(~ 1 ~ 1, data=upsa_occUMF, se=TRUE)


# Create a fit list or a list of all models to be used in mod selection
fits <- list(
  upsa_occ.1=upsa_occ.1,
  upsa_occ.2=upsa_occ.2,
  upsa_occ.int=upsa_occ.int
)

# load the AICcmodavg package
#install.packages("AICcmodavg")
library(AICcmodavg)

# Create an AICc table using the AICcmodavg package
#(second.ord=False specifies to use AIC only and not AICc)
aicc <- aictab(fits, modnames=c(1:3), sort=TRUE, second.ord=TRUE)

# Look at 'best model' outputs
upsa_occ.1

# Estimate of occupancy probability for average landcover site in year 3
mod1lc <- linearComb(upsa_occ.1,c(-2.186,0,0,0,0,0,3),'state')

# Backtransform estimate to probability scale
plogis(mod1lc@estimate)

# make sequence of predictions to plot marginal grass effect
newData <- data.frame(allgrss100 = -2:2, allgrss100sq = (-2:2)^2, trees100 = 0, trees100sq = 0, year = factor("3",levels = c("1","2","3")))
pred_df <- predict(upsa_occ.1, type = 'state', newdata = newData, appendData=TRUE)
plot(newData$allgrss100,pred_df$Predicted, type = "l")

