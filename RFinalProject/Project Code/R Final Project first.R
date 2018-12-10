## Open up data files
BJK <- read.csv("~/Desktop/Final Project ETF data/BJK.csv")
`^GSPC` <- read.csv("~/Desktop/Final Project ETF data/^GSPC.csv")
IGN <- read.csv("~/Desktop/Final Project ETF data/IGN.csv")
FUD <- read.csv("~/Desktop/Final Project ETF data/FUD.csv")
PAGG <- read.csv("~/Desktop/Final Project ETF data/PAGG.csv")
PBJ <- read.csv("~/Desktop/Final Project ETF data/PBJ.csv")
PBS <- read.csv("~/Desktop/Final Project ETF data/PBS.csv")
PEJ <- read.csv("~/Desktop/Final Project ETF data/PEJ.csv")
XLP <- read.csv("~/Desktop/Final Project ETF data/XLP.csv")
XLY <- read.csv("~/Desktop/Final Project ETF data/XLY.csv")

## Create log-return variables
logreturn_SP <- log(`^GSPC`$Close/`^GSPC`$Open)
summary(logreturn_SP)

logreturn_BJK <- log(BJK$Close/BJK$Open)
summary(logreturn_BJK)

logreturn_IGN <- log(IGN$Close/IGN$Open)
summary(logreturn_IGN)

logreturn_FUD <- log(FUD$Close/FUD$Open)
summary(logreturn_FUD)

logreturn_PAGG <- log(PAGG$Close/PAGG$Open)
summary(logreturn_PAGG)

logreturn_PBJ <- log(PBJ$Close/PBJ$Open)
summary(logreturn_PBJ)

logreturn_PBS <- log(PBS$Close/PBS$Open)
summary(logreturn_PBS)

logreturn_PEJ <- log(PEJ$Close/PEJ$Open)
summary(logreturn_PEJ)

logreturn_XLP <- log(XLP$Close/XLP$Open)
summary(logreturn_XLP)

logreturn_XLY <- log(XLY$Close/XLY$Open)
summary(logreturn_XLY)

## Runs test to determine randomness

install.packages("randtests")
library(randtests)

runs.test(logreturn_BJK)
runs.test(logreturn_SP)
runs.test(logreturn_FUD)
runs.test(logreturn_IGN)
runs.test(logreturn_PAGG)
runs.test(logreturn_PBJ)
runs.test(logreturn_PBS)
runs.test(logreturn_PEJ)
runs.test(logreturn_XLP)
runs.test(logreturn_XLY)


## Create histograms and Normal Probability Plots

hist(logreturn_SP, 
     main="Histogram for SP Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.07, 0.07, by=0.01)
     )

qqnorm(logreturn_SP)
qqline(logreturn_SP)   # Not normal

hist(logreturn_BJK, 
     main="Histogram for BJK Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.13, 0.08, by=0.01)
)

qqnorm(logreturn_BJK) # Not normal
qqline(logreturn_BJK)

hist(logreturn_IGN, 
     main="Histogram for IGN Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.06, 0.06, by=0.01)
)

qqnorm(logreturn_IGN)
qqline(logreturn_IGN)

hist(logreturn_FUD, 
     main="Histogram for FUD Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.18, 0.08, by=0.01)
)

qqnorm(logreturn_FUD, main = "FUD Log Return Normal Probability Plot")
qqline(logreturn_FUD) # Not normal

hist(logreturn_PAGG, 
     main="Histogram for PAGG Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.18, 0.08, by=0.02)
)

qqnorm(logreturn_PAGG, main = "PAGG Log Return Normal Probability Plot") # Not normal
qqline(logreturn_PAGG)

hist(logreturn_PBJ, 
     main="Histogram for PBJ Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.04, 0.04, by=0.005)
)

qqnorm(logreturn_PBJ)
qqline(logreturn_PBJ)

hist(logreturn_PBS, 
     main="Histogram for PBS Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.1, 0.1, by=0.01)
)

qqnorm(logreturn_PBS)
qqline(logreturn_PBS)

hist(logreturn_PEJ, 
     main="Histogram for PEJ Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.12, 0.07, by=0.01)
)

qqnorm(logreturn_PEJ)
qqline(logreturn_PEJ)

hist(logreturn_XLP, 
     main="Histogram for XLP Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.05, 0.05, by=0.01)
)

qqnorm(logreturn_XLP)
qqline(logreturn_XLP)

hist(logreturn_XLY, 
     main="Histogram for XLY Log Returns", 
     xlab="Log Returns", 
     col="blue",
     breaks = seq(-0.07, 0.07, by=0.01)
)

qqnorm(logreturn_XLY)
qqline(logreturn_XLY)

## Construct confidence intervals for means and variances

# 95% CI for SP mean
# Use t score with n - 1 = 2498 degrees of freedom

df <- length(logreturn_BJK) - 1
n <- nrow(BJK)

margin_SP_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_SP)/sqrt(n)
upper_SP_mean = mean(logreturn_SP) + margin_SP_mean
lower_SP_mean = mean(logreturn_SP) - margin_SP_mean
c(mean = mean(logreturn_SP), lower = lower_SP_mean, upper = upper_SP_mean)

margin_BJK_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_BJK)/sqrt(n)
upper_BJK_mean = mean(logreturn_BJK) + margin_BJK_mean
lower_BJK_mean = mean(logreturn_BJK) - margin_BJK_mean
c(mean = mean(logreturn_BJK), lower = lower_BJK_mean, upper = upper_BJK_mean)

margin_PBJ_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_PBJ)/sqrt(n)
upper_PBJ_mean = mean(logreturn_PBJ) + margin_PBJ_mean
lower_PBJ_mean = mean(logreturn_PBJ) - margin_PBJ_mean
c(mean = mean(logreturn_PBJ), lower = lower_PBJ_mean, upper = upper_PBJ_mean)

margin_FUD_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_FUD)/sqrt(n)
upper_FUD_mean = mean(logreturn_FUD) + margin_FUD_mean
lower_FUD_mean = mean(logreturn_FUD) - margin_FUD_mean
c(mean = mean(logreturn_FUD), lower = lower_FUD_mean, upper = upper_FUD_mean)

margin_IGN_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_IGN)/sqrt(n)
upper_IGN_mean = mean(logreturn_IGN) + margin_IGN_mean
lower_IGN_mean = mean(logreturn_IGN) - margin_IGN_mean
c(mean = mean(logreturn_IGN), lower = lower_IGN_mean, upper = upper_IGN_mean)

margin_PAGG_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_PAGG)/sqrt(n)
upper_PAGG_mean = mean(logreturn_PAGG) + margin_PAGG_mean
lower_PAGG_mean = mean(logreturn_PBJ) - margin_PBJ_mean
c(mean = mean(logreturn_PAGG), lower = lower_PAGG_mean, upper = upper_PAGG_mean)

margin_PBS_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_PBS)/sqrt(n)
upper_PBS_mean = mean(logreturn_PBS) + margin_PBS_mean
lower_PBS_mean = mean(logreturn_PBS) - margin_PBS_mean
c(mean = mean(logreturn_PBS), lower = lower_PBS_mean, upper = upper_PBS_mean)

margin_PEJ_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_PEJ)/sqrt(n)
upper_PEJ_mean = mean(logreturn_PEJ) + margin_PEJ_mean
lower_PEJ_mean = mean(logreturn_PEJ) - margin_PEJ_mean
c(mean = mean(logreturn_PEJ), lower = lower_PEJ_mean, upper = upper_PEJ_mean)

margin_XLP_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_XLP)/sqrt(n)
upper_XLP_mean = mean(logreturn_XLP) + margin_XLP_mean
lower_XLP_mean = mean(logreturn_XLP) - margin_XLP_mean
c(mean = mean(logreturn_XLP), lower = lower_XLP_mean, upper = upper_XLP_mean)

margin_XLY_mean = qt(0.025, df, lower.tail = FALSE) * sd(logreturn_XLY)/sqrt(n)
upper_XLY_mean = mean(logreturn_XLY) + margin_XLY_mean
lower_XLY_mean = mean(logreturn_XLY) - margin_XLY_mean
c(mean = mean(logreturn_XLY), lower = lower_XLY_mean, upper = upper_XLY_mean)

# 95% CI for SP variance
# ((n-1)S^2/chi^2(0.025, 2498), (n-1)S^2/chi^2(0.975, 2498))

lower_SP_var = var(logreturn_SP) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_SP_var = var(logreturn_SP) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_SP), lower = lower_SP_var, upper = upper_SP_var)

lower_BJK_var = var(logreturn_BJK) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_BJK_var = var(logreturn_BJK) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_BJK), lower = lower_BJK_var, upper = upper_BJK_var)

lower_FUD_var = var(logreturn_FUD) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_FUD_var = var(logreturn_FUD) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_FUD), lower = lower_FUD_var, upper = upper_FUD_var)

lower_IGN_var = var(logreturn_IGN) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_IGN_var = var(logreturn_IGN) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_IGN), lower = lower_IGN_var, upper = upper_IGN_var)

lower_PAGG_var = var(logreturn_FUD) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_PAGG_var = var(logreturn_FUD) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_PAGG), lower = lower_PAGG_var, upper = upper_PAGG_var)

lower_PBJ_var = var(logreturn_PBJ) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_PBJ_var = var(logreturn_PBJ) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_PBJ), lower = lower_PBJ_var, upper = upper_PBJ_var)

lower_PBS_var = var(logreturn_PBS) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_PBS_var = var(logreturn_PBS) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_PBS), lower = lower_PBS_var, upper = upper_PBS_var)

lower_PEJ_var = var(logreturn_PEJ) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_PEJ_var = var(logreturn_PEJ) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_PEJ), lower = lower_PEJ_var, upper = upper_PEJ_var)

lower_XLP_var = var(logreturn_XLP) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_XLP_var = var(logreturn_XLP) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_XLP), lower = lower_XLP_var, upper = upper_XLP_var)

lower_XLY_var = var(logreturn_XLY) * df / qchisq(0.05/2, df, lower.tail = FALSE)
upper_XLY_var = var(logreturn_XLY) * df / qchisq(1 - 0.05/2, df, lower.tail = FALSE)
c(variance = var(logreturn_XLY), lower = lower_FUD_var, upper = upper_FUD_var)

## Time series regression of log-returns on time

tslogreturn_BJK <- ts(logreturn_BJK, frequency = 1)
tstime_BJK <- ts(BJK$Date)
summary(lm(tslogreturn_BJK~tstime_BJK))

tslogreturn_SP <- ts(logreturn_SP, frequency = 1)
tstime_SP <- ts(`^GSPC`$Date)
summary(lm(tslogreturn_SP~tstime_SP))

tslogreturn_FUD <- ts(logreturn_FUD, frequency = 1)
tstime_FUD <- ts(FUD$Date)
summary(lm(tslogreturn_FUD~tstime_FUD))

tslogreturn_IGN <- ts(logreturn_IGN, frequency = 1)
tstime_IGN <- ts(FUD$Date)
summary(lm(tslogreturn_IGN~tstime_IGN))

tslogreturn_PAGG <- ts(logreturn_PAGG, frequency = 1)
tstime_PAGG <- ts(PAGG$Date)
summary(lm(tslogreturn_PAGG~tstime_PAGG))

tslogreturn_PBJ <- ts(logreturn_PBJ, frequency = 1)
tstime_PBJ <- ts(PBJ$Date)
summary(lm(tslogreturn_PBJ~tstime_PBJ))

tslogreturn_PBS <- ts(logreturn_PBS, frequency = 1)
tstime_PBS <- ts(PBS$Date)
summary(lm(tslogreturn_PBS~tstime_PBS))

tslogreturn_PEJ <- ts(logreturn_PEJ, frequency = 1)
tstime_PEJ <- ts(PEJ$Date)
summary(lm(tslogreturn_PEJ~tstime_PEJ))

tslogreturn_XLP <- ts(logreturn_XLP, frequency = 1)
tstime_XLP <- ts(XLP$Date)
summary(lm(tslogreturn_XLP~tstime_XLP))

tslogreturn_XLY <- ts(logreturn_XLY, frequency = 1)
tstime_XLY <- ts(XLY$Date)
summary(lm(tslogreturn_XLY~tstime_XLY))

## Test the equality of conditional means

# Create the Year dummy variable and compute conditional means

year <- substring(BJK$Date,1,4)
year_num <- as.numeric(as.character(year))
dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)


# Test equality 

`^GSPC`$dummy_year <- NA
`^GSPC`$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
`^GSPC`$logreturn_SP <- log(`^GSPC`$Close/`^GSPC`$Open)
summary(`^GSPC`$logreturn_SP)

t.test(subset(`^GSPC`$logreturn_SP, dummy_year == 1),subset(`^GSPC`$logreturn_SP, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(`^GSPC`$logreturn_SP, dummy_year == 1),subset(`^GSPC`$logreturn_SP, dummy_year == 0), alternative = c("less"), var.equal = FALSE)


BJK$dummy_year <- NA
BJK$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
BJK$logreturn_BJK <- log(BJK$Close/BJK$Open)
summary(BJK$logreturn_BJK)

t.test(subset(BJK$logreturn_BJK, dummy_year == 1),subset(BJK$logreturn_BJK, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(BJK$logreturn_BJK, dummy_year == 1),subset(BJK$logreturn_BJK, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

FUD$dummy_year <- NA
FUD$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
FUD$logreturn_FUD <- log(FUD$Close/FUD$Open)
summary(FUD$logreturn_FUD)

t.test(subset(FUD$logreturn_FUD, dummy_year == 1),subset(FUD$logreturn_FUD, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(FUD$logreturn_FUD, dummy_year == 1),subset(FUD$logreturn_FUD, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

IGN$dummy_year <- NA
IGN$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
IGN$logreturn_IGN <- log(IGN$Close/IGN$Open)
summary(IGN$logreturn_IGN)

t.test(subset(IGN$logreturn_IGN, dummy_year == 1),subset(IGN$logreturn_IGN, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(IGN$logreturn_IGN, dummy_year == 1),subset(IGN$logreturn_IGN, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

PAGG$dummy_year <- NA
PAGG$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
PAGG$logreturn_PAGG <- log(PAGG$Close/PAGG$Open)
summary(PAGG$logreturn_PAGG)

t.test(subset(PAGG$logreturn_PAGG, dummy_year == 1),subset(PAGG$logreturn_PAGG, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(PAGG$logreturn_PAGG, dummy_year == 1),subset(PAGG$logreturn_PAGG, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

PBJ$dummy_year <- NA
PBJ$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
PBJ$logreturn_PBJ <- log(PBJ$Close/PBJ$Open)
summary(PBJ$logreturn_PBJ)

t.test(subset(PBJ$logreturn_PBJ, dummy_year == 1),subset(PBJ$logreturn_PBJ, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(PBJ$logreturn_PBJ, dummy_year == 1),subset(PBJ$logreturn_PBJ, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

PBS$dummy_year <- NA
PBS$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
PBS$logreturn_PBS <- log(PBS$Close/PBS$Open)
summary(PBS$logreturn_PBS)

t.test(subset(PBS$logreturn_PBS, dummy_year == 1),subset(PBS$logreturn_PBS, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(PBS$logreturn_PBS, dummy_year == 1),subset(PBS$logreturn_PBS, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

PEJ$dummy_year <- NA
PEJ$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
PEJ$logreturn_PEJ <- log(PEJ$Close/PEJ$Open)
summary(PEJ$logreturn_PEJ)

t.test(subset(PEJ$logreturn_PEJ, dummy_year == 1),subset(PEJ$logreturn_PEJ, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(PEJ$logreturn_PEJ, dummy_year == 1),subset(PEJ$logreturn_PEJ, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

XLP$dummy_year <- NA
XLP$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
XLP$logreturn_XLP <- log(XLP$Close/XLP$Open)
summary(XLP$logreturn_XLP)

t.test(subset(XLP$logreturn_XLP, dummy_year == 1),subset(XLP$logreturn_XLP, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(XLP$logreturn_XLP, dummy_year == 1),subset(XLP$logreturn_XLP, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

XLY$dummy_year <- NA
XLY$dummy_year <- ifelse((year_num == 2009) |(year_num == 2011)|(year_num == 2012)|(year_num == 2018), 1, 0)
XLY$logreturn_XLY <- log(XLY$Close/XLY$Open)
summary(XLY$logreturn_XLY)

t.test(subset(XLY$logreturn_XLY, dummy_year == 1),subset(XLY$logreturn_XLY, dummy_year == 0), alternative = c("less"), var.equal = TRUE)
t.test(subset(XLY$logreturn_XLY, dummy_year == 1),subset(XLY$logreturn_XLY, dummy_year == 0), alternative = c("less"), var.equal = FALSE)

## Run regression on the Year dummy variable 

summary(lm(logreturn_SP~dummy_year))
summary(lm(logreturn_BJK~dummy_year))
summary(lm(logreturn_FUD~dummy_year))
summary(lm(logreturn_IGN~dummy_year))
summary(lm(logreturn_PAGG~dummy_year))
summary(lm(logreturn_PBJ~dummy_year))
summary(lm(logreturn_PBS~dummy_year))
summary(lm(logreturn_PEJ~dummy_year))
summary(lm(logreturn_XLP~dummy_year))
summary(lm(logreturn_XLY~dummy_year))




