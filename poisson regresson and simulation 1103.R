##Generalized Linear Models

#######

## possible useful packages to install
## ggplot2
install.packages("ggplot2")

## sandwich
install.packages("sandwich")

## msm
install.packages("msm")

## require packages
require(ggplot2)

require(sandwich)

require(msm)

## qqplot included in ggplot2;



## try to simulate the Poisson Regression of Fathers' ages and
## the mutation rate in a certain gene
## 



##### Read in a table of mutation rate and parental ages
## from local file
data <- read.table("denovo_age_data.txt", header = TRUE, sep = "\t")
summary(data)

plot(data$AgeFatherAtConception, data$nDNM, main = "FatherAge vs Denovo Mutation", 
     xlab = "fathers' age", ylab = "denovo mutation")

## add fit lines
abline( lm( data$nDNM ~ data$AgeFatherAtConception), col = "blue") 
lines(lowess(data$nDNM, data$AgeFatherAtConception), col = "red")

age <- data$AgeFatherAtConception
mute <- data$nDNM


## fit mutations against father's age
fitMuteAge <- glm(mute ~ age, family = "poisson")
summary(fitMuteAge)


## coeff
coeff <- coef(fitMuteAge)
coeff

par(mfrow=c(1,1))
plot(age, mute)
xvalues <- sort(age)
log_mean <- coeff[1] + coeff[2] * xvalues

mean_value <- exp(log_mean)
lines(xvalues, mean_value)

plot(age, mute)
lines(xvalues, mean_value)

## Q-Q plot
par(mfrow=c(2,2))
plot(fitMuteAge)


#####################################
## model allowing for overdispersion:

fit.overdis <- glm(mute ~ age, family = quasipoisson)
summary(fit.overdis)

## coefficient
coeff.overdis <- coef(fit.overdis)
coeff.overdis

par(mfrow = c(1, 1))

plot(age, mute)
xvalues <-sort(age)
log_mean.overdis <- coeff.overdis[1] + coeff.overdis[2] * xvalues

mean_value.overdis <- exp(log_mean.overdis)
lines(xvalues, mean_value.overdis)







#######################
## fit log-mutations against father's age
## this does not work, ==! 
#mute.log <- as.integer(mute.log)
mute.log <- log(mute)
mute.log

fitLogMute <- glm(mute.log ~ age, family = "poisson")
summary(fitLogMute)

## coeff of log mutations against age
coeff.log <- coef(fitLogMute)
coeff.log

plot(age, mute.log)
x_values.log <- sort(age)

mean.log <- coeff.log[1] + coeff.log[2] * x_values.log
mean_value.log <- exp(mean.log)
lines(x_values.log, mean_value.log)


## Q-Q plot
par(mfrow=c(2,2))
plot(fitMuteAge)

plot(fitLogMute)


############################################################
# mutation rate for gene_X
lambda <- runif(1000,5,9)

# generate fathers' age, a normal distribution (35, 6)
age <- rnorm(1000, 35, 6)


# number of mutations (count) in gene_X
# here we try three different methods to generate mutations
X1 <- rpois(1000,mean(lambda))
X2 <- rpois(1000,lambda)
X3 <- rpois(1000, age)

# try two different offset(), see which one would fit well
l.MR <- rep(log(mean(lambda)),1000)

l2.MR <- log(lambda)


## fit mutations with different offsets

# fit X1 to two offsets
fitX1_mLamb <- glm(X1~offset(l.MR),family="poisson")
fitX1_Lamb <- glm(X1 ~ offset(l2.MR), family="poisson")

# fit X2 to two offsets
fitX2_mLamb <- glm( X2~offset(l.MR), family = "poisson")
fitX2_Lamb <- glm( X2~offset(l2.MR), family = "poisson")

# fit X3 to two offsets
fitX3_mLamb <- glm( X3~offset(l.MR), family = "poisson")   ## family = "poisson")
fitX3_Lamb <- glm( X3~offset(l2.MR), family = "poisson")   ## family = "poisson")


##fit4 <- glm( X3~offset(l.MR), family = "poisson")
##fit5 <- glm( X3~offset(l2.MR), family = "poisson")


## summary fit1 - fit5
summary(fitX1_mLamb)
summary(fitX1_Lamb)

summary(fitX2_mLamb)
summary(fitX2_Lamb)

summary(fitX3_mLamb)
summary(fitX3_Lamb)



## plot all fited models
# plot fitX1 to two offsets
par(mfrow=c(2,2))
plot(fitX1_mLamb)

par(mfrow=c(2,2))
plot(fitX1_Lamb)


# plot fit X2 to two offsets
par(mfrow=c(2,2))
plot(fitX2_mLamb)

par(mfrow=c(2,2))
plot(fitX2_Lamb)


# plot fit X3 to two offsets
par(mfrow=c(2,2))
plot(fitX3_mLamb)

par(mfrow=c(2,2))
plot(fitX3_Lamb)






#########################################
#########################################
## NOT useful below, still working on these plotting code

fitted(fit1)


## cal the Intercept for fit
cov.fit <- vcovHC(fit, type = "HC0")
cov.fit <- vcovHC(fit, type="HC0")
std.err <- sqrt(diag(cov.fit))
r.est <- cbind(Estimate = coef(fit), "Robust SE" = std.err,
               "Pr(>|z|)" = 2 * pnorm( abs( coef(fit)/std.err), lower.tail = FALSE),
               LL = coef(fit) - 1.96 * std.err, 
               UL = coef(fit) + 1.96 * std.err)

r.est

##
with(fit, cbind(res.deviance = deviance, df = df.residual,
                p = pchisq(deviance, df.residual, lower.tail=FALSE)))

fit_update <- update(fit2, . ~ . - X2)

##anova
anova(fit_update, fit, test = "Chisq")




## cal the Intercept for fit2
cov.fit2 <- vcovHC(fit2, type = "HC0")
cov.fit2 <- vcovHC(fit2, type="HC0")
std.err <- sqrt(diag(cov.fit2))
r2.est <- cbind(Estimate = coef(fit2), "Robust SE" = std.err,
                "Pr(>|z|)" = 2 * pnorm( abs( coef(fit2)/std.err), lower.tail = FALSE),
                LL = coef(fit2) - 1.96 * std.err, 
                UL = coef(fit2) + 1.96 * std.err)

r2.est

##
with(fit2, cbind(res.deviance = deviance, df = df.residual,
                 p = pchisq(deviance, df.residual, lower.tail=FALSE)))

##anova(fit2, fit, test = "Chisq")
anova(fit, test = "Chisq")
anova(fit2, test = "Chisq")
anova(fit3, test = "Chisq")

##drop1()
drop1(fit, test = "Chisq")


plot(fit)

plot(fit2)

plot(fit3)
