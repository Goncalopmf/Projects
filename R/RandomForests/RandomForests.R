############# Import Packages #############

pacman::p_load(pacman, ggplot2, cowplot, randomForest) 

library(ggplot2)
library(cowplot)
library(randomForest)

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

# To reproduce our results, specify seed
set.seed(42)

# Impute values for the NAs in the dataset
data.imputed <- rfImpute(hd ~ ., # predict hd based on all of the other columns
                         data = data,
                         iter = 6 # number of random forests built to estimate missing values
                         ) 

# After each iteration, rfImpute prints out the Out-Of-Bag (OOB) error rate
# which should get smaller if the estimates improve
# Returns how many samples were wrongly classified

############# Build a Random Forest #############
model <- randomForest(hd ~ ., # predict hd based on all of the other columns
                      data = data.imputed, 
                      proximity = TRUE # returns the proximity matrix
                      )

model
# The returns from this command are:
# 1) The OOB error rate for the forest with ntree trees. 
#    In this case ntree=500 by default
# 2) The confusion matrix for the forest with ntree trees.

# Verify if 500 trees is enough for optimal classification
# Create a dataframe that formats the error rate matrix for ggplot
model$err.rate

oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times = 3), # column for the number of trees
  Type=rep(c('OOB','Healthy','Unhealthy'), each=nrow(model$err.rate)), # column for the type of error
  Error=c(model$err.rate[,'OOB'],  # column for the error value
          model$err.rate[,'Healthy'],
          model$err.rate[,'Unhealthy'])
)

oob.error.data

ggplot(data = oob.error.data,
       aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))
# Blue line shows the error when classifying unhealthy patients
# Green line shows the overall OOB error rate
# Red line shows the error rate when classifying healthy patients
# Overall, when ntrees increases, the error rate decreases

# To test with 1000 trees:
model <- randomForest(hd ~ ., # predict hd based on all of the other columns
                      data = data.imputed, 
                      ntree = 1000,
                      proximity = TRUE # returns the proximity matrix
)

model

# Plot the error rates again
oob.error.data <- data.frame(
  Trees=rep(1:nrow(model$err.rate), times = 3), # column for the number of trees
  Type=rep(c('OOB','Healthy','Unhealthy'), each=nrow(model$err.rate)), # column for the type of error
  Error=c(model$err.rate[,'OOB'],  # column for the error value
          model$err.rate[,'Healthy'],
          model$err.rate[,'Unhealthy'])
)

oob.error.data

ggplot(data = oob.error.data,
       aes(x=Trees, y=Error))+
  geom_line(aes(color=Type))
# We see that after 500 trees, the error rate stabilizes


# Now, verify if we are using the optimal number of variables at each internal note in the tree
model # Return: No. of variables tried at each split: 3

# Create an empty vector
oob.values <- vector(length = 10) 

# Create a loop that tests different number of variables at each step 
for(i in i:10){
  temp.model <- randomForest(hd ~ .,  # build random forest with mtry = i, between 1 and 10
                             data = data.imputed,
                             mtry = i,
                             ntree = 1000
                             )
  
  # access the value in the last rowand in the first column
  # i.e, the OOB error rate when all 1000 trees have been made
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

oob.values # we see that when i = 2, we have the lowest OOB, so 2 is the optimal value

# Now, use the random forest to draw an MDS plot with samples
# This shows us how they are related to each other

# Create distance matrix from 1 - proximity matrix
distance.matrix <- dist(1 - model$proximity)

# MDS with the distance matrix
mds <- cmdscale(distance.matrix,
                eig = TRUE,
                x.ret = TRUE
                )

mds

# Calculate the percentage of variation in the distance matrix that the X and Y axes account for
mds.var.per <- round(mds$eig/sum(mds$eig)*100, 1)

# Format the data for ggplot -> create dataframe
mds.values <- mds$points

mds.data <- data.frame(Sample=rownames(mds.values),
                       X = mds.values[,1],
                       Y = mds.values[,2],
                       Status = data.imputed$hd # the status of 'healthy' or 'unhealthy' for each sample
                       )

#mds.data$Status

ggplot(data = mds.data,
       aes(x=X, y=Y, label=Sample)) +
  geom_text(aes(color=Status)) + # instead of dots, plot the status 
  theme_bw() + # white background 
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")

