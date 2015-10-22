##Lab 3B: Foundations for inference - confidence levels

load(url("http://www.openintro.org/stat/data/ames.RData"))
population <- ames$Gr.Liv.Area
samp <- sample(population,60)

head(population)
hist(population)
head(samp)
hist(samp)

#check the mean value of samp
sample_mean <- mean(samp)
sample_mean

#SE is the value of SD/Sqrt(count)
se <-sd(samp)/sqrt(60)
lower <- sample_mean - 1.96 * se
upper <- sample_mean + 1.96 * se

## the confidence interval 
c(lower, upper)
lower
upper


## recreate many samples to know sample means and confidence intervals
samp_mean <- rep(NA, 50)
samp_sd <- rep(NA, 50)
n <- 60
samp_mean
samp_sd

for(i in 1:50){
  samp <- sample(population, n) # obtain a sample of size n = 60 from the population
  samp_mean[i] <- mean(samp)    # save sample mean in ith element of samp_mean
  samp_sd[i] <- sd(samp)        # save sample sd in ith element of samp_sd
}

samp
samp_mean
samp_sd

#construct the confidence intervals
lower <- samp_mean - 1.96 * samp_sd / sqrt(n)
upper <- samp_mean + 1.96 * samp_sd / sqrt(n)

## 50 confidence intervals are stored in lower, 50 confidence intervals are stored in upper
lower
upper

c(lower[1], upper[1])

## plot the first interval:
plot_ci(lower, upper, mean(population))

