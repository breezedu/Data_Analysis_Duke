##Generalized Linear Models

#######

## ggplot2
install.packages("ggplot2")
require(ggplot2)

## sandwich
install.packages("sandwich")
require(sandwich)


## msm
install.packages("msm")
require(msm)


lambda <- runif(1000,5,9)

X1 <- rpois(1000,mean(lambda))
X2 <- rpois(1000,lambda)

l.MR <- rep(log(mean(lambda)),1000)

fit <- glm(X1~offset(l.MR),family="poisson")

fit2 <- glm( X2~offset(l.MR), family = "poisson")

fit3 <- glm( X2~offset(log(lambda)), family = "poisson")   ## family = "poisson")


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