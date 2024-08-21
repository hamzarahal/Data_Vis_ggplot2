############################ Introducing ggplot2 ###################################
####################################################################################

# Reading Datasets with read_csv


install.packages("tidyverse")
install.packages("ggmap")

# Load the Tidyverse
library(tidyverse)

# Read the college dataset
college <- read_csv('http://672258.youcanlearnit.net/college.csv')

# Take a look at the data
summary(college)

# Convert state, region, highest_degree, control, and gender to factors
college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender))

# Take a look at the data
summary(college)

# What's going on with loan_default_rate?
unique(college$loan_default_rate)

# Let's just force that to numeric and the "NULL" will convert to N/A
college <- college %>%
  mutate(loan_default_rate=as.numeric(loan_default_rate))

# Take a look at the data
summary(college)

# Building First Visualization

college <- college %>%
  mutate(state=as.factor(state), region=as.factor(region),
         highest_degree=as.factor(highest_degree),
         control=as.factor(control), gender=as.factor(gender),
         loan_default_rate=as.numeric(loan_default_rate))

# Calling ggplot() along just creates a blank plot
ggplot()

# I need to tell ggplot what data to use
ggplot(data=college)

# And then give it some instructions using the grammar of graphics.
# Let's build a simple scatterplot with tuition on the x-axis and average SAT score on the y axis
ggplot(data=college) +
  geom_point(mapping=aes(x=tuition, y=sat_avg))
