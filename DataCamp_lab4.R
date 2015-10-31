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
