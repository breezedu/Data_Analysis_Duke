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
plot(fitX1_mLamb)

plot(fitX1_Lamb)


# plot fit X2 to two offsets
plot(fitX2_mLamb)

plot(fitX2_Lamb)


# plot fit X3 to two offsets
plot(fitX3_mLamb)

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
