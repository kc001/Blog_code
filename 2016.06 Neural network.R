# https://archive.ics.uci.edu/ml/datasets/Bank+Marketing
raw.data <- read.csv("bank-additional-full.csv", header = TRUE, sep = ";")

str(raw.data)

data <- raw.data

# Remove "duration", as this variable would not be known before a call is initiated
data$duration <- NULL

# Remove number of days since last contact" variable, 
# for which most values are "999" (no previous contact) and captured by the "previous" variable
data$pdays <- NULL


###########################################################
# Use min-max normalization to re-scale numeric variables #
###########################################################

num.vars <- data[, sapply(data, is.numeric)]
maxs <- apply(num.vars, 2, max) 
mins <- apply(num.vars, 2, min)

processed <- as.data.frame(scale(num.vars, center = mins, scale = maxs - mins))
apply(processed, 2, range) # Check that all variable are rescaled from 0-1
remove(mins, maxs, num.vars)


###############################
# Dummy-code factor variables #
###############################

# Select factor variables for dummy-coding
vars <- data[, sapply(data, is.factor)]
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


##############################
# Add y to processed dataset #
##############################

table(data$y)
sum(is.na(data$y))
processed$y <- factor(data$y,levels=c("yes", "no"),ordered=TRUE)
table(processed$y)
levels(processed$y)


##################################
# Create train and test datasets #
##################################

library(caret)
set.seed(1)
inTrain <- createDataPartition(y = processed$y, # the outcome variable
                               p = 0.75, # the percentage of data in the training sample
                               list = FALSE) # should the results be in a list?
train <- processed[inTrain,]
test <- processed[-inTrain,]
remove(inTrain)


#################################
# Run neural network using nnet #
#################################

library(nnet)

set.seed(1)
model.nn <- nnet(y ~ ., data=train, size=10, maxit=1000, decay=.01, linout=FALSE)

preds.nn <- predict(model.nn, newdata=test, type="class")

results.nn <- table(predicted=preds.nn, true=test$y)
results.nn

# overall accuracy = sum of diagonal elements / sum of all elements
accuracy.nn <- round(sum(diag(results.nn[nrow(results.nn):1, ]))/sum(results.nn), 3)

# sensitivity in predicting responders
sensitivity.nn <- round(results.nn["yes","yes"]/sum(results.nn[,"yes"]), 3)

# specificity in predicting non-responders
specificity.nn <- round(results.nn["no","no"]/sum(results.nn[,"no"]), 3) 


##################################
# Run neural network using caret #
##################################

library(nnet)
library(caret)

# Grid of tuning parameters to try:
fitGrid <- expand.grid(.size=c(5,10,15), .decay=c(0.001,0.01,0.1))

# Set the seeds
# Necessary for reproducibility because we're using parallel processing
set.seed(1)
seeds <- vector(mode = "list", length = 6) # number of resamples + 1 for final model
for(i in 1:5) seeds[[i]] <- sample.int(n=1000, 9) # 9 is the number of tuning parameter combinations
seeds[[6]] <- 1 # for the last model
remove(i)
seeds

library(pROC)
fitControl <- trainControl(method = "cv", # use N-fold cross validation
                           number = 5, # the number of folds
                           classProbs = TRUE, summaryFunction = twoClassSummary,
                           seeds = seeds)

library(doParallel)
cl = makeCluster(6)
registerDoParallel(cl)

#Fit model
model.ct.nn <- train(y ~ ., 
                     data=train, 
                     method='nnet', 
                     maxit = 1000, 
                     linout = FALSE,
                     trControl = fitControl,
                     tuneGrid = fitGrid,
                     metric = "Sens", # maximize sensitivity to "yes" values
                     allowParallel = TRUE) 

stopCluster(cl)
remove(cl)
registerDoSEQ()

model.ct.nn


############################################
# Generate predictions from caret-run nnet #
############################################

preds.ct.nn <- predict(model.ct.nn, newdata=test)

results.ct.nn <- table(predicted=preds.ct.nn, true=test$y)
results.ct.nn

# overall accuracy = sum of diagonal elements / sum of all elements
accuracy.ct.nn <- round(sum(diag(results.ct.nn))/sum(results.ct.nn), 3)

# sensitivity in predicting responders
sensitivity.ct.nn <- round(results.ct.nn["yes","yes"]/sum(results.ct.nn[,"yes"]), 3)

# specificity in predicting non-responders
specificity.ct.nn <- round(results.ct.nn["no","no"]/sum(results.ct.nn[,"no"]), 3) 


########################
# Graph neural network #
########################

library(NeuralNetTools)
plotnet(model.ct.nn)

# Subset data to run simplified model to generate a sample graph
graph.data <- subset(train, 
                     select = c("age","campaign","previous","emp.var.rate","cons.price.idx","cons.conf.idx",
                                "euribor3m","nr.employed","y"))

model.graph.nn <- nnet(y ~ ., data=graph.data, size=1, maxit=1000, decay=.01, linout=FALSE)
plotnet(model.graph.nn)