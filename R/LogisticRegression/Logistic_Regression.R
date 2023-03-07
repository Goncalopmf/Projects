############# Import Packages #############


pacman::p_load(pacman, ggplot2, cowplot) 

library(ggplot2)
library(cowplot)

############# Load the Dataset #############
# we use a dataset from the UCI machine learning repository
# http://archive.ics.uci.edu/ml/datasets/Heart+Disease
############# We want to predict if a patient has heart disease or not #########

url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"
data <- read.csv(url, header=FALSE)
head(data) # columns are not labeled

# label the columns, see information for each column in the repository
colnames(data) <- c(
  "age",
  "sex",# 0 = female, 1 = male
  "cp", # chest pain
  # 1 = typical angina,
  # 2 = atypical angina,
  # 3 = non-anginal pain,
  # 4 = asymptomatic
  "trestbps", # resting blood pressure (in mm Hg)
  "chol", # serum cholestoral in mg/dl
  "fbs",  # fasting blood sugar if less than 120 mg/dl, 1 = TRUE, 0 = FALSE
  "restecg", # resting electrocardiographic results
  # 1 = normal
  # 2 = having ST-T wave abnormality
  # 3 = showing probable or definite left ventricular hypertrophy
  "thalach", # maximum heart rate achieved
  "exang",   # exercise induced angina, 1 = yes, 0 = no
  "oldpeak", # ST depression induced by exercise relative to rest
  "slope", # the slope of the peak exercise ST segment
  # 1 = upsloping
  # 2 = flat
  # 3 = downsloping
  "ca", # number of major vessels (0-3) colored by fluoroscopy
  "thal", # this is short of thalium heart scan
  # 3 = normal (no cold spots)
  # 6 = fixed defect (cold spots during rest and exercise)
  # 7 = reversible defect (when cold spots only appear during exercise)
  "hd" # (the predicted attribute) - diagnosis of heart disease
  # 0 if less than or equal to 50% diameter narrowing
  # 1 if greater than 50% diameter narrowing
)

head(data)

# Verify the structure of the data
str(data)
# we see that sex is a number, but should be a factor 
# cp is suposed to also be a factor
# ca and thal both have levels '?'
# Thus, we need to clean these problems




############# Format the Data #############
data[data== '?'] <- NA # Change ? to NA

data[data$sex == 0,]$sex <- "F" # convert the 0s in sex to F, for female
data[data$sex == 1,]$sex <- "M" # convert the 0s in sex to F, for female
data$sex <- as.factor(data$sex)
# conver the remaining columns that should be factors
data$cp <- as.factor(data$cp)
data$fbs <- as.factor(data$fbs)
data$restecg <- as.factor(data$restecg)
data$exang <- as.factor(data$exang)
data$slope <- as.factor(data$slope)

data$ca <- as.integer(data$ca) # R thinks tat the levels for the factor are strings
# since the column had "?" values. However, the documentation shows that they are integers
# thus, we need to convert the strings to integers
data$ca <- as.factor(data$ca)  # finally, convert the integers to factor levels again

data$thal <- as.integer(data$thal) # same process as column ca
data$thal <- as.factor(data$thal)

## Replace 0 and 1 with "Healthy" and "Unhealthy" for hd column
data$hd <- ifelse(test=data$hd == 0, yes="Healthy", no="Unhealthy")
data$hd <- as.factor(data$hd) # Now convert to a factor

# Finally, verify again the structure of the data
str(data)

# Now, verify how many samples (rows) have NA values
nrow(data[is.na(data$ca) | is.na(data$thal),]) # these columns had '?' that were replaced with NA
# View these samples
data[is.na(data$ca) | is.na(data$thal),]
nrow(data)
# Since the number of samples is small compared to the dataset (303), we can remove these 6 samples
data <- data[!(is.na(data$ca) | is.na(data$thal)),]
# verify that the rows with NA values were removed
nrow(data) 
str(data)

# Now we need to verify that healthy and diseased samples come from each gender
# This serves as a quality control, since if only male samples have heart disease (hd),
# we should probably remove all females from the model, or we risk introducing severe biases
xtabs(~ hd + sex, # use model syntax to select the columns in the dataset 
      data = data)
# we see that healthy and unhealthy are represented by both female and male samples

# Now verify that all 4 levels of chest pain (cp) were reported by a variety of patients
xtabs(~ hd + cp, # use model syntax to select the columns in the dataset 
      data = data)

# And now do the same thing for all the boolean and categorical variables that will be
# used to predict heart disease

xtabs(~ hd + restecg, data=data)
# here, only 4 patients represent level 1, which could influence negatively finding the 
# best fitting line. Lets see if its problematic after
xtabs(~ hd + exang, data=data)
xtabs(~ hd + slope, data=data)
xtabs(~ hd + ca, data=data)
xtabs(~ hd + thal, data=data)
xtabs(~ hd + fbs, data=data)

############# Build a Simple Logistic Regression Model #############
# First, we create a simple model that uses sex to predict heart disease

xtabs(~ hd + sex, data=data)
# here, we see that most of the females are healthy and most of the males are unhealthy
# Being female is likely to decrease the odds in being unhealthy.
#   In other words, if a sample is female, the odds are against it that it
#   will be unhealthy

# build the logistic regression
logistic <- glm(hd ~ sex, # use sex to predict hd
                data=data, 
                family = 'binomial'
                )
# details about the logistic regression
summary(logistic)

# The equation here is:
# heart disease = -1.0438 + 1.2737 x the patient is male
# where the intercept is the log(odds) a female will be unhealthy. This is because
# female is the first factor in "sex"

# Calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
#paste(ll.null)
ll.proposed <- logistic$deviance/-2
#paste(ll.proposed)
# McFadden's Pseudo R^2
(ll.null - ll.proposed) / ll.null # = 0.05812569 - very low value, probably a bad predictor model
# p-value
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1)

# Now, see predictions from the logistic regression model
predicted.data <- data.frame( # create dataframe that has the probabilities of having hd
                              # along with the actual heart disease status
                            probability.of.hd=logistic$fitted.values, # 1st column
                            sex=data$sex
                            )
head(predicted.data) 

# Plot the predictions
ggplot(data=predicted.data, aes(x=sex, y=probability.of.hd)) +
  geom_point(aes(color=sex), size=5) + # geom_point draws the data
  xlab("Sex") +
  ylab("Predicted probability of getting heart disease")

############# Build a Complete Logistic Regression Model #############
logistic <- glm(hd ~ .,  # here, the model uses all variables to predict hd
                data=data, 
                family="binomial")
summary(logistic)
# large p-values are bad predictors

# Calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
#paste(ll.null)
ll.proposed <- logistic$deviance/-2
#paste(ll.proposed)
# McFadden's Pseudo R^2
(ll.null - ll.proposed) / ll.null # = 0.5533531 - much higher than for the simple model
# p-value
1 - pchisq(2*(ll.proposed - ll.null), df=1) # = 0, results are statistically significant
1 - pchisq((logistic$null.deviance - logistic$deviance), df=1) # = 0

# Plot the data
predicted.data <- data.frame( # create dataframe that has the probabilities of having hd
                              # along with the actual heart disease status
                            probability.of.hd=logistic$fitted.values,
                            hd=data$hd
                            )

predicted.data <- predicted.data[ # sort the dataframe from low probabilities to high probs
  order(predicted.data$probability.of.hd, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data) # add a new column that has the rank of each sample
                                              # from low probability to high probability

ggplot(data=predicted.data, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=1) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

#ggsave("Path/heart_disease_probabilities.pdf") # edit path
