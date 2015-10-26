## R data analysis examples: Poisson Regression

## ggplot2
install.packages("ggplot2")
require(ggplot2)

## sandwich
install.packages("sandwich")
require(sandwich)


## msm
install.packages("msm")
require(msm)


## read data from an url:
p <- read.csv("http://www.ats.ucla.edu/stat/data/poisson_sim.csv")

## description of the data
p <- within(p, {
  prog <- factor(prog, levels = 1:3, labels = c("General", "Academic", "Vocational"))
  id <- factor(id)
})

summary(p)

## call tapply function to display the summary statistics by program type
with(p, tapply(num_awards, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


## ggplot
ggplot(p, aes(num_awards, fill = prog)) +
  geom_histogram(binwidth=.5, position="dodge")

## Fit the model and store it in object m1
summary(m1 <- glm(num_awards ~ prog + math, family = "poisson", data = p))

