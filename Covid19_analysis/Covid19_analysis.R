rm(list=ls())

pacman::p_load(pacman, pastecs)

library(pastecs)

data <- read.csv("~/Desktop/Projects_ML/R/Covid19_analysis/COVID19_line_list_data.csv")
stat.desc(data) 

############# Clean up the dataset #############

# Some of the entries in the death column are dates, while most are 0 or 1
# 0 - person survived
# 1 - person died 
unique(data$death)
# to fix this, we every entry that corresponds to death = True (1 or a date) is set to 1
data$death_dummy <- as.integer(data$death != 0) # coerces the argument to be of interger type
# verify again, should only have 0 and 1
unique(data$death_dummy)

# Analyse the death rate
death_rate = sum(data$death_dummy) / nrow(data)

##### What if we want to check if people who die are older than people who survive?
##### To see this, the average age of the dead group of people should be higher than the group that survived
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)

dead_mean = mean(dead$age)
paste('The average age of the people that died is = ', dead_mean)
alive_mean = mean(alive$age)
paste('The average age of the people that survived is = ', alive_mean)

# We get 'NA' for both variables 
unique(dead$age) # we see NA value
unique(alive$age) # we see NA value
 
# to solve this problem, we should exclude rows with 'NA' - use na.remove command
dead_mean = mean(dead$age, na.rm = TRUE)
paste('The average age of the people that died is = ', dead_mean)
alive_mean = mean(alive$age, na.rm = TRUE)
paste('The average age of the people that survived is = ', alive_mean)

# Is this result statistically significant? Verify with t-test 
t.test(alive$age,
       dead$age,
       alternative = 'two.sided', # the difference in means is not zero
       conf.level = 0.95 # 2-standard deviation
       ) 
# we get a very small p value (2.2e-16) - reject the null hypothesis
# pretty much there is a 0% chance that the 2 populations have the same mean age

##### Now, see if there is a gender disparity in the ages of the people that died
# man
men = subset(data, gender == 'male')
mean(men$death_dummy, na.rm = TRUE) # 8.46%
# women
women = subset(data, gender == 'female')
mean(women$death_dummy, na.rm = TRUE) # 3.67%
# we find that men have almost the double of the death rate of the women
# is this statistically significant? Again, use t-test to verify
t.test(men$death_dummy,
       women$death_dummy,
       alternative = 'two.sided', # the difference in means is not zero
       conf.level = 0.95 # 2-standard deviation
      ) 
# 95% confidence: men have from 1.7% to 7.9% higher chance of dying
# p-value = 0.002 < 0.05, so this is statistically significant

