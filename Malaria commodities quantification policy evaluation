#################################################
#      MRC_Data_science Program                 #
#    Mathematical Modelling Projects            #
#################################################

#Trends and seasonal variation analysis and 
#mathematical modelling of childhood illnesses in Rwanda from 2012 to 2016
#Malaria, neumonia and diarrhea among children under five in IMCI program 

data <- read.csv("/Users/ughe-student/Documents/MRC-ITS/ITS/MALARIA_COMMODITIES.csv",header=T) 

View(data)

summary(data)
colnames(data)
#Create a ts object from Malaria commodities data in the data.frame

Cortem6x3STKOUTts <- ts(data$Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays, 
                        start=c(2012, 1), end=c(2016, 12), frequency=12)
Cortem6x3STKOUTts

QuinVialSTOKOUTts <- ts(data$Quinine.vial.300.mgml_StkOutDays, 
                      start=c(2012, 1), end=c(2016, 12), frequency=12)
QuinVialSTOKOUTts

QuinTabSTOKOUTts <- ts(data$Quinine.tab.300.mg_StkOutDays, 
                      start=c(2012, 1), end=c(2016, 12), frequency=12)
QuinTabSTOKOUTts




#EXploratory data analysis
par(mfrow=c(1,2)) #this puts 2 graphs on one page, in one row, 2 columns

#Create a histogram and boxplot for the malaria data

x <- Cortem6x3STKOUTts 
h<-hist(x, breaks=10, xlab="Coartem distribution figures", 
        main="Disctribution of Coartem tablets distribution data") #Malaria data is not normally distributed
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

boxplot(Cortem6x3STKOUTts, xlab="Coartem distribution figures", 
        main="Disctribution of Coartem tablets distribution data")

#Create a histogram and boxplot for the Quinine Vial stockout data

x <- QuinVialSTOKOUTts
h<-hist(x, breaks=10, xlab="Quinine Vial stockout figures", 
        main="Disctribution of Quinine Vial stockout data") #Malaria data is not normally distributed
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

boxplot(QuinVialSTOKOUTts, xlab="Quinine Vial stockout figures", 
        main="Disctribution of Quinine Vial stockout")

#Create a histogram and boxplot for the Quinine Tablet data

x <- QuinTabSTOKOUTts
h<-hist(x, breaks=10, xlab="Quinine Tablet stockout figures", 
        main="Disctribution of Quinine Tablet stockout data") #Malaria data is not normally distributed
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

boxplot(QuinTabSTOKOUTts, xlab="Quinine Tablet stockout figures", 
        main="Disctribution of Quinine Tablet stockout data")

#Does the avarege morbidity appear to have chenged over time(Years)? 
#We will need to view the data on anual basis

#Normal distribution
qqnorm(Cortem6x3STKOUTts,
       ylab="Sample Quantiles for stockout")
qqline(Cortem6x3STKOUTts, 
       col="red")

library(MASS)

Box = boxcox(Cortem6x3STKOUTts ~ 1,              # Transform Turbidity as a single vector
             lambda = seq(-6,6,0.1)      # Try values -6 to 6 by 0.1
)

Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y

Cox2[1,]                                  # Display the lambda with the greatest

#    log likelihood

lambda = Cox2[1, "Box.x"]                 # Extract that lambda

T_box = (Cortem6x3STKOUTts ^ lambda - 1)/lambda   # Transform the original data


library(rcompanion)

plotNormalHistogram(T_box)

T_box
plot(T_box)

library(lattice)

#Anaual Coartem Stockout data analysis
histogram(~data$Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays|as.factor(Year), data=data)
summary(Cortem6x3STKOUTts)
par(mfrow=c(1,2))
plot(data$Year, data$Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays)
plot(as.factor(data$Year), data$Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays)

#Try transformed data (T_box)
par(mfrow=c(1,2))
plot(data$Year, T_box)
plot(as.factor(data$Year), T_box)


#Anaual Quine Vial Stockout data analysis
histogram(~data$Quinine.vial.300.mgml_StkOutDays|as.factor(Year), data=data)
par(mfrow=c(1,2))
plot(data$Year, data$Quinine.vial.300.mgml_StkOutDays)
plot(as.factor(data$Year), data$Quinine.vial.300.mgml_StkOutDays)

#Anaual Quinine Tab stockout data analysis
histogram(~data$Quinine.tab.300.mg_StkOutDays|as.factor(Year), data=data)
par(mfrow=c(1,2))
plot(data$Year, data$Quinine.tab.300.mg_StkOutDays)
plot(as.factor(data$Year), data$Quinine.tab.300.mg_StkOutDays)

#Normalizing Quinine Tab data with Cox-Box Power transformation approach
Box = boxcox(QuinTabSTOKOUTts ~ 1,              # Transform Turbidity as a single vector
             lambda = seq(-6,6,0.1)      # Try values -6 to 6 by 0.1
)

Cox = data.frame(Box$x, Box$y)            # Create a data frame with the results

Cox2 = Cox[with(Cox, order(-Cox$Box.y)),] # Order the new data frame by decreasing y

Cox2[1,]                                  # Display the lambda with the greatest

#    log likelihood

lambda = Cox2[1, "Box.x"]                 # Extract that lambda

T_box = (QuinTabSTOKOUTts ^ lambda - 1)/lambda   # Transform the original data


library(rcompanion)

plotNormalHistogram(T_box)

T_box
plot(T_box)
#Try transformed data (T_box)
par(mfrow=c(1,2))
plot(data$Year, T_box)
plot(as.factor(data$Year), T_box)

#Plot stockout seasonal patterns
layout(1:3)
par(mar = rep(2, 4))
boxplot(Cortem6x3STKOUTts~cycle(Cortem6x3STKOUTts), xlab="Time (Months)", 
        ylim=c(0,max(Cortem6x3STKOUTts)+10),
        main="Coartem stockout")
boxplot(QuinVialSTOKOUTts~cycle(QuinVialSTOKOUTts), xlab="Time (Months)", 
        ylim=c(0,max(QuinVialSTOKOUTts)+10),
        main="Quinine Vial stockout")
boxplot(QuinTabSTOKOUTts~cycle(QuinTabSTOKOUTts), 
        ylim=c(0,max(QuinTabSTOKOUTts)+10),
        xlab="Time (Months)", main="Quinine Tablet stockout")
T_box
boxplot(T_box~cycle(T_box), xlab="Time (Months)", ylim=c(0,max(T_box)+10),
        main="Coartem stockout")


#PLotting the trend of stockout, loss and distribution of commodities
layout(1:3)
plot(Cortem6x3STKOUTts, ylab = "Coartem stockout trend")
plot(QuinVialSTOKOUTts, ylab = "Quinine Vial stockout trend")
plot(QuinTabSTOKOUTts, ylab = "Quinine Tablet stockout trend")

#PLotting the trend of stockout, loss and distribution of commodities after removal of seasonal variation 

layout(1:3)
plot(aggregate(Cortem6x3STKOUTts), ylab = "Coartem stockout trend, seasonality removed")
plot(aggregate(QuinVialSTOKOUTts), ylab = "Quinine Vial stockout trend, seasonality removed")
plot(aggregate(QuinTabSTOKOUTts), ylab = "Quinine Tablet stockout trend, seasonality removed")

#Decompose stockout, loss and distribution of commodities in their components (trend, seasonality and irregularity)
plot(decompose(Cortem6x3STKOUTts))
Stockout.decom <- decompose(Cortem6x3STKOUTts, type = "mult")
Stockout.decom #The highest peak for seasonal variation is in Dec-Jan and May-June while trough is in Aug-Sept
plot(Stockout.decom)

Trend <- Stockout.decom$trend
Seasonal <- Stockout.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)

plot(decompose(QuinVialSTOKOUTts))
QuinVial.decom <- decompose(QuinVialSTOKOUTts, type = "mult")
QuinVial.decom #The highest peak is in May for seasonal variation and trough in August-September
plot(QuinVial.decom)
Trend <- QuinVial.decom$trend
Seasonal <- QuinVial.decom$seasonal
ts.plot(cbind(Trend, Trend * Seasonal), lty = 1:2)

# Load the necessary libraries
library(nlme)
library(car)
library(foreign)
library(tseries)

# Make the trend interaction variable (alternative to Excel)
data$level <- data$policy
data$time <- 1:nrow(data)
data$trend <- (data$time - 30) * data$level

# Plot the time series for the first outcome variable
layout(1:3)


plot(data$time,Cortem6x3STKOUTts,
     ylab="Quinine Vial 300 stockout",
     ylim=c(150,max(data$Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays)+10),
     xlab="Months",
     type="l",
     col="red",
     xaxt="n")
# Add x-axis year labels
axis(1, at=1:60, labels=data$Period)


# Add in the points for the figure

points(data$time,Cortem6x3STKOUTts,
       col="red",
       pch=20)

# Label the policy
abline(v=31,lty=2)

colnames(data)
plot(data$time,data$Quinine.vial.300.mgml_StkOutDays,
     ylab="Quinine Vial 300 stockout",
     ylim=c(150,max(data$Quinine.vial.300.mgml_StkOutDays)+10),
     xlab="Months",
     type="l",
     col="red",
     xaxt="n")
# Add x-axis year labels
axis(1, at=1:60, labels=data$Period)
# Add in the points for the figure

points(data$time,data$Quinine.vial.300.mgml_StkOutDays,
       col="red",
       pch=20)

# Label the policy
abline(v=31,lty=2)
plot(data$time,data$Quinine.tab.300.mg_StkOutDays,
     ylab="Quinine Vial 300 stockout",
     ylim=c(150,max(data$Quinine.tab.300.mg_StkOutDays)+10),
     xlab="Months",
     type="l",
     col="red",
     xaxt="n")
# Add x-axis year labels
axis(1, at=1:60, labels=data$Period)
# Add in the points for the figure

points(data$time,data$Quinine.tab.300.mg_StkOutDays,
       col="red",
       pch=20)

# Label the policy
abline(v=31,lty=2)

# Modeling
# A preliminary OLS regression
ols1 <- lm(Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays ~ time + level + trend, data=data)
summary(ols1)
ols2 <- lm(Quinine.vial.300.mgml_StkOutDays ~ time + level + trend, data=data)
summary(ols2)
ols3 <- lm(Quinine.tab.300.mg_StkOutDays ~ time + level + trend, data=data)
summary(ols3)

## A graph of the residuals from the OLS regression to check if there is serially correlated errors
dwt(ols1,max.lag=10,alternative="two.sided")
plot(data$time, residuals(ols),type='o',pch=16,xlab='Time',ylab='OLS Residuals')
abline(h=0,lty=2)

dwt(ols2,max.lag=10,alternative="two.sided")
plot(data$time, residuals(ols2),type='o',pch=16,xlab='Time',ylab='OLS Residuals')
abline(h=0,lty=2)

dwt(ols3,max.lag=10,alternative="two.sided")
plot(data$time, residuals(ols3),type='o',pch=16,xlab='Time',ylab='OLS Residuals')
abline(h=0,lty=2)

##Check the autocorrelation and partial-autocorrelation functions 
##Determine the appropriate lag p in ARMA model 
par(mfrow=c(2,1))
acf(residuals(ols1))
acf(residuals(ols1),type='partial')
# No concerns about autocorrelation
par(mfrow=c(2,1))
acf(residuals(ols2))
acf(residuals(ols2),type='partial')
#No concerns about autocorrelation

# Fit the ARMA regression model
# We use the gls function to fit the regression model with
# a variety of correlated-error and non-constant error-variance structures
model_on1 <- gls(Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays ~ time + level + trend,
                data=data, correlation=corARMA(p=1,form=~time), method="ML")
summary(model_on1)

model_on2 <- gls(Quinine.vial.300.mgml_StkOutDays ~ time + level + trend,
                      data=data, correlation=corARMA(p=1,form=~time), method="ML")
summary(model_on2)

model_on3 <- gls(Quinine.tab.300.mg_StkOutDays ~ time + level + trend,
                 data=data, correlation=corARMA(p=1,form=~time), method="ML")
summary(model_on3)

# Diagnostic tests

# Likelihood-ratio tests to check whether the parameters of the AR process for the errors are necessary and sufficient
model_af_1 <- update(model_on1,correlation=corARMA(p=2,form=~time))
anova(model_on1,model_af)

model_af2_1 <- update(model_on1,correlation=corARMA(p=1,q=1,form=~time))
anova(model_on,model_af2)

model_af_2 <- update(model_on2,correlation=corARMA(p=2,form=~time))
anova(model_on2,model_af_2)

model_af2_2 <- update(model_on2,correlation=corARMA(p=1,q=1,form=~time))
anova(model_on2,model_af2_2)

model_af_3 <- update(model_on3,correlation=corARMA(p=2,form=~time))
anova(model_on3,model_af_3)

model_af2_3 <- update(model_on3,correlation=corARMA(p=1,q=1,form=~time))
anova(model_on3,model_af2_3)


# Residual plot
# Null Hypo: the residuals of a correctly specified model are independently distributed--the residuals are white noise
par(mfrow=c(2,1))
acf(residuals(model_on1),form=~time)
qqPlot(residuals(model_on1))

par(mfrow=c(2,1))
acf(residuals(model_on2),form=~time)
qqPlot(residuals(model_on2))

par(mfrow=c(2,1))
acf(residuals(model_on3),form=~time)
qqPlot(residuals(model_on3))

# Perform a Box test on the residuals
Box.test(residuals(model_on),1)
Box.test(residuals(model_on2),1)


colnames(data)
#PLOTTING THE MODELS
# Produce the plot, first plotting the raw data points
layout(1:3)
par(mfrow=c(1,3))
with(data,
     plot(time,Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays,
          ylim=c(0,max(Coartem.Artemether.Lumefanthrine.tab.20.mg.120mg.6x3.StkOutDays)+10),
          ylab="Coartem stockout", main="Coartem stockout",
          xlab="Year",
          pch=20,
          col="grey",
          xaxt="n")
)


# Plot dates and the launch of the intervention
axis(1, at=1:60, labels=data$Period)
abline(v=31,lty="dotted")


# Plot the first line segment
lines(data$time[1:30], fitted(model_on1)[1:30], col="red",lwd=2)
# And the counterfactual
segments(1, model_on1$coef[1]+model_on1$coef[2],60,model_on1$coef[1]+model_on1$coef[2]*60,lty=2,col='red',lwd=2)

# Plot the second line segment
lines(data$time[31:60], fitted(model_on1)[31:60], col="red",lwd=2)

# Produce the plot, first plotting the raw data points

with(data,
     plot(time,Quinine.vial.300.mgml_StkOutDays,
          ylim=c(0,max(Quinine.vial.300.mgml_StkOutDays)+10),
          ylab="Quinine Vial stockout", main="Quinine Vial stockout",
          xlab="Year",
          pch=20,
          col="grey",
          xaxt="n")
)


# Plot dates and the launch of the intervention
axis(1, at=1:60, labels=data$Period)
abline(v=31,lty="dotted")


# Plot the first line segment
lines(data$time[1:30], fitted(model_on2)[1:30], col="red",lwd=2)
# And the counterfactual
segments(1, model_on2$coef[1]+model_on2$coef[2],60,model_on2$coef[1]+model_on2$coef[2]*60,lty=2,col='red',lwd=2)

# Plot the second line segment
lines(data$time[31:60], fitted(model_on2)[31:60], col="red",lwd=2)


with(data,
     plot(time,Quinine.tab.300.mg_StkOutDays,
          ylim=c(0,max(Quinine.tab.300.mg_StkOutDays)+10),
          ylab="Quinine Tablet stockout", main="Quinine Tablet stockout",
          xlab="Year",
          pch=20,
          col="grey",
          xaxt="n")
)


# Plot dates and the launch of the intervention
axis(1, at=1:60, labels=data$Period)
abline(v=31,lty="dotted")


# Plot the first line segment
lines(data$time[1:30], fitted(model_on3)[1:30], col="red",lwd=2)
# And the counterfactual
segments(1, model_on3$coef[1]+model_on3$coef[2],60,model_on3$coef[1]+model_on3$coef[2]*60,lty=2,col='red',lwd=2)

# Plot the second line segment
lines(data$time[31:60], fitted(model_on3)[31:60], col="red",lwd=2)


# Predicted value at 20 years after the intervention
pred <- model_on1$coef[1] + model_on1$coef[2]*48 + model_on1$coef[3] + model_on1$coef[4]*20
pred
# Or, more directly
pred <- fitted(model_on1)[48]

# Then estimate the counterfactual at the same time point
cfac <- model_on1$coef[1] + model_on1$coef[2]*48

# Absolute change at 2 years
pred - cfac
# Relative change at 2 years
(pred - cfac) / cfac

# END
