# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
data <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";")
str(data)

# Remove "duration", as this variable would not be known before a call is initiated
data$duration <- NULL

# Remove number of days since last contact" variable, 
# for which most values are "999" (no previous contact) and captured by the "previous" variable
data$pdays <- NULL

# Re-order dependent variable factor levels for caret
table(data$y)
data$y <- factor(data$y, levels=c("yes", "no"), ordered=FALSE)
table(data$y)
levels(data$y)

# Determine which observations should be in the train and test datasets
library(caret)
set.seed(1)
inTrain <- createDataPartition(y = data$y, # the outcome variable
                               p = 0.75, # the percentage of data in the training sample
                               list = FALSE) # should the results be in a list?


#################
# Neural network #
##################

# Use min-max normalization to re-scale numeric variables
num.vars <- data[, sapply(data, is.numeric)]
maxs <- apply(num.vars, 2, max) 
mins <- apply(num.vars, 2, min)

processed <- as.data.frame(scale(num.vars, center = mins, scale = maxs - mins))
apply(processed, 2, range) # Check that all variable are rescaled from 0-1
remove(mins, maxs, num.vars)

# Dummy-code factor variables
vars <- data[, sapply(data, is.factor)] # Select factor variables for dummy-coding
vars$y <- NULL # Remove the dependent variable, which we will want to be a factor
vars <- colnames(vars)

library(psych)
for (i in 1:length(vars) ) {
    
    var <- vars[i]
    
    new <- dummy.code(data[,var]) # convert factor to dummy variables
    
    # Rename dummy variables
    new <- dummy.code(data[,var])
    names <- colnames(new)
    names <- paste(var, names, sep = ".")
    colnames(new) <- names
    remove(names)
    
    processed <- data.frame(processed, new)
    remove(new)
    
}

remove(i, vars, var)

# Add y to processed dataset
processed$y <- data$y
table(processed$y)
levels(processed$y)

# Create train and test datasets 
train.nn <- processed[inTrain,]
test.nn <- processed[-inTrain,]


###############################
# Run neural network in caret #
###############################

library(nnet)
library(caret)

# Grid of tuning parameters to try:
nn.Grid <- expand.grid(.size=c(5,10,15), .decay=c(0.001,0.01,0.1))

# Set the seeds
# Necessary for reproducibility because we're using parallel processing
set.seed(1)
nn.seeds <- vector(mode = "list", length = 11) # number of resamples + 1 for final model
for(i in 1:10) nn.seeds[[i]] <- sample.int(n=1000, 9) # 9 is the number of tuning parameter combinations
nn.seeds[[11]] <- 1 # for the last model
remove(i)
nn.seeds

library(pROC)
nn.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                           number = 5, # the number of folds
                           repeats = 2,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           seeds = nn.seeds)


library(doParallel)
cl = makeCluster(6)
registerDoParallel(cl)

#Fit model
model.nn <- train(y ~ ., 
                  data=train.nn, 
                  method='nnet', 
                  maxit = 1000, 
                  linout = FALSE,
                  trControl = nn.Control,
                  tuneGrid = nn.Grid,
                  metric = "Sens",
                  allowParallel = TRUE) 

stopCluster(cl)
remove(cl)
registerDoSEQ()

model.nn
plot(model.nn, metric = "Sens")

remove(nn.Control, nn.Grid, nn.seeds, processed)


#################
# Random Forest #
#################

# randomForest can only accommodate factor variables with 32 or fewer levels
# Check whether any any of the factor variables have more than 32 levels; they don't.
too.many.levels <- function(x) {
    is.factor(x) == TRUE & length(levels(x)) > 32
}
delete <- lapply(data, too.many.levels)
remove(delete, too.many.levels)

# Create train and test datasets
train <- data[inTrain,]
test <- data[-inTrain,]


##############################
# Run random forest in caret #
##############################

library(caret)
library(randomForest)

# default number of predictors is predictors/3 or about 6
rf.Grid <- expand.grid(mtry = seq(from = 3, to = 18, by = 3))

set.seed(1)
rf.seeds <- vector(mode = "list", length = 11) # length is = (nresampling)+1
for(i in 1:10) rf.seeds[[i]]<- sample.int(n=1000, 6) # 6 is the number of tuning parameters (mtry possibilities)
rf.seeds[[11]] <- 1 # for the last model
remove(i)
rf.seeds

library(pROC)
rf.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                           number = 5, # the number of folds
                           repeats = 2,
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           seeds = rf.seeds)

library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)

model.rf <- train(train[, -19], train$y, 
                  method="rf",
                  ntree=100,
                  importance=TRUE, 
                  na.action=na.omit,
                  tuneGrid = rf.Grid,
                  trControl= rf.Control,
                  metric = "Sens", 
                  allowParallel=TRUE)

stopCluster(cl)
remove(cl)
registerDoSEQ()

remove(rf.Grid, rf.Control, rf.seeds)

# Examine results
print(model.rf$finalModel) 
importance(model.rf$finalModel)
plot(model.rf, metric = "Sens")


#######################
# Logistic regression #
#######################

# Quick-and-dirty predictor selection
importance <- as.data.frame(importance(model.rf$finalModel))
importance <- importance[order(-importance$MeanDecreaseAccuracy),]

importance$vars <- rownames(importance)
rownames(importance) <- NULL

vars <- importance[1:10, "vars"]

f <- as.formula(paste("y ~", paste(vars, collapse = " + ")))
remove(vars, importance)


#################################
# Run logistic regression model #
#################################

set.seed(1)
log.seeds <- vector(mode = "list", length = 11) # number of resamples + 1 for final model
for(i in 1:10) log.seeds[[i]] <- sample.int(n=1000, 1) # 1 because not adjusting any tuning parameters - just randomly sampling
log.seeds[[11]] <- 1 # for the last model
remove(i)
log.seeds

library(pROC)
log.Control <- trainControl(method = "repeatedcv", # use N-fold cross validation
                            number = 5, # the number of folds
                            repeats = 2,
                            classProbs = TRUE, summaryFunction = twoClassSummary,
                            seeds = log.seeds)

library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)

model.log <- train(f,
                   data=train, 
                   method="glm", 
                   family=binomial(link='logit'),
                   trControl= log.Control)
summary(model.log)

stopCluster(cl)
remove(cl)
registerDoSEQ()

remove(log.Control, log.seeds, f)


##################
# Compare models #
##################

library(caret)
# collect resamples
results <- resamples(list(RandomForest=model.rf, NeuralNetwork=model.nn, LogisticRegression=model.log))
results$values

# summarize the distributions
summary(results)

# boxplot of results
bwplot(results, metric="Sens")

bwplot(results, metric="Spec")
bwplot(results, metric="ROC")
remove(results)


####################
# Test predictions #
####################

# Generate predicted classes
preds.log <- predict.train(model.log, newdata=test, type="raw") # Logistic regression
preds.nn <- predict.train(model.nn, newdata=test.nn, type="raw") # Neural network - remember that you're using processed data
preds.rf <- predict.train(model.rf, newdata=test, type="raw") # Random forest

results.nn <- confusionMatrix(preds.nn, test$y, positive = "yes")
results.log <- confusionMatrix(preds.log, test$y, positive = "yes")
results.rf <- confusionMatrix(preds.rf, test$y, positive = "yes")

results.nn
results.log
results.rf