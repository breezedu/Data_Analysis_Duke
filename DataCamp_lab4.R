## DataCamp Lab4
## Duke Statistic


# Part A: North Carolina births
# In 2004, the state of North Carolina released a large data set 
# containing information on births recorded in this state. 
# This data set is useful to researchers studying the relation between habits 
# and practices of expectant mothers and the birth of their children. 
# We will work with a random sample of observations from this data set.

# Exploratory analysis

# Load the nc data set into our workspace.
load(url("http://bit.ly/dasi_nc"))
head(nc)

# summary
summary(nc)

## create a cleaned-up version of the weight gain variable
gained_clean = na.omit(nc$gained)

# the length of gained_clean
n = length(gained_clean)


# create a new object boot_means to store bootstrap means
boot_means = rep(NA, 100)


# take 100 bootstrap samples, record their means in boot_means
for(i in 1:100){
  boot_sample = sample(gained_clean, n, replace = TRUE)
  boot_means[i] = mean(boot_sample)                       
}

summary(boot_sample)


## the inference function
source("http://bit.ly/dasi_inference")

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.90, est = "mean", boot_method = "perc")


inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "perc")


inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")


inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "median", boot_method = "se")


by(nc$weight, nc$habit, mean)


inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")


inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical", order = c("smoker","nonsmoker"))
