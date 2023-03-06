############# Import Packages #############

pacman::p_load(pacman, pROC, randomForest) 

library(pROC) # library to draw ROC graphs
library(randomForest) # usr random forests

set.seed(420) # deterministic outputs

############# Generate Dataset #############

num.samples <- 100
weight <- sort(rnorm(n = num.samples, # normal distribution
                     mean = 172, # mean value
                     sd = 29 # standard deviation
                     ) 
               ) # generate column using normal distribution
#weight

# classify an individual as obese or not obese
obese <- ifelse(test=(runif(n=num.samples) < # random numbers between 0 and 1
                        (rank(weight)/100)), # rank weights from lightest to heaviest and scale them by 100
                yes=1, no=0) # if the random number is smaller than the scaled rank, individual is obese (=1)
#obese

############# Plot the Data #############
plot(x=weight, y=obese)

glm.fit=glm(obese ~ weight, family = binomial) # fit a logistic regression to the data
lines(weight, 
      glm.fit$fitted.values # this contains estimated probabilities that each sample is obese
      ) # pass weight and the fitted.values into the lines function

############# Draw an ROC Curve #############
roc(obese, glm.fit$fitted.values,
    plot = TRUE # plot the graph
    ) 
