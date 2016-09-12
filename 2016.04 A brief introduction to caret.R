# Import file
loan <- read.csv("LoanStats3a.csv")


#########################################################
# Prepare data - this part is the same as the last post #
#########################################################

# Set missing data to NA
is.na(loan) <- loan == ""

# Create date variables that R recognizes as dates
loan$issue_d <- as.Date(paste('15', loan$issue_d), format='%d %b-%y')
loan$earliest_cr_line <- as.Date(paste('15', loan$earliest_cr_line), format='%d %b-%y')

# Identify loans that have already come to term
loan$term.months <- NA
loan$term.months[loan$term==" 36 months"] <- 36
loan$term.months[loan$term==" 60 months"] <- 60
loan$term <- NULL # the "term" variable is redundant with the numerical term.months variable

library(lubridate)
loan$maturity.date <- loan$issue_d + months(loan$term.months)

today <- Sys.Date()
loan$mature <- ifelse(loan$maturity.date < today, 1, 0)
loan$maturity.date <- NULL
remove(today)

loan <- subset(loan, mature==1) # subset data to select only mature loans

# Convert character percentages to numeric variables
loan$int_rate <- as.numeric(gsub("%" , "", loan$int_rate))
loan$revol_util <- as.numeric(gsub("%" , "", loan$revol_util))

# Convert character employment length to numeric variable
# This produces some missing data for values of emp_length that were "n/a"
loan$emp_length <- gsub(" years" , "", loan$emp_length)
loan$emp_length <- gsub(" year" , "", loan$emp_length)
loan$emp_length <- ifelse(loan$emp_length == "10+", 10, loan$emp_length)
loan$emp_length <- ifelse(loan$emp_length == "< 1", 0.5, loan$emp_length)
loan$emp_length <- as.numeric(loan$emp_length)

# Convert character to ordinal variable
loan$grade[loan$grade == ""] <- NA
loan$grade <- ordered(loan$grade)

# Remove variables where more than 20% of the observations are missing values
loan <- loan[, colMeans(is.na(loan)) <= .20]

# Remove factors with too many levels, as randomForest can only accommodate 32 levels
too.many.levels <- function(x) {
    is.factor(x) == TRUE & length(levels(x)) > 32
}
delete <- lapply(loan, too.many.levels)
loan <- loan[, delete == FALSE]
remove(too.many.levels, delete)

# Percentage of loan paid back
loan$paid.back <- (loan$funded_amnt - loan$out_prncp)/loan$funded_amnt
hist(loan$paid.back)
range(loan$paid.back, na.rm = TRUE)

# Remove accounts with missing paid.back status, as we won't be able to model them
loan <- subset(loan, ! is.na(loan$paid.back))

# There are a couple of variables that provide additional outcome data about the loan, 
# so including them in the model as predictors doesn't realistically model 
# the challenge of predicting which loans will be paid back.
loan$last_pymnt_amnt <- NULL # Last total payment amount received
loan$total_pymnt <- NULL # Payments received to date for total amount funded
loan$total_pymnt_inv <- NULL # Payments received to date for portion of total amount funded by investors
loan$total_rec_prncp <- NULL # total recovered principal
loan$out_prncp <- NULL # Remaining outstanding principal for total amount funded
loan$out_prncp_inv <- NULL # Remaining outstanding principal for portion of total amount funded by investors
loan$total_rec_int <- NULL # Interest received to date
loan$total_rec_late_fee <- NULL # Late fees received to date
loan$collection_recovery_fee <- NULL # post charge off collection fee
loan$recoveries <- NULL # amount recovered after loan is charged off
loan$loan_status <- NULL
loan$last_pymnt_d <- NULL # Last month payment was received
loan$next_pymnt_d <- NULL # Next scheduled payment date
loan$last_credit_pull_d <- NULL # most recent month LC pulled credit for this loan

# Remove variables where all values are the same
loan <- loan[sapply(loan, function(x) length(levels(factor(x)))>1)]

# check the amount of missing data in remaining dataset
lapply(loan, function(x) { sum(is.na(x)) })

# Remove ID number so randomForest doesn't try to use it as a predictor
loan$member_id <- NULL

# Simplify things by using only complete cases
loan <- loan[complete.cases(loan),]


#################################
# Training and Testing Datasets #
#################################

library(caret)
set.seed(1)
inTrain <- createDataPartition(y = loan$paid.back, # the outcome variable
                               p = 0.75, # the percentage of data in the training sample
                               list = FALSE) # should the results be in a list?

# the caret package has a function named "train", so we'll call the training dataset training instead.
training <- loan[inTrain,]
testing <- loan[-inTrain,]
remove(inTrain)

# Check that stratification creates equal distributions
hist(training$paid.back)
hist(testing$paid.back)


######################
# randomForest model #
######################

# randomForest package
library(randomForest)
set.seed(1)
# mtry = number of variables randomly sampled as candidates at each split; default is p/3 for regression, or 9
# nodesize = size of terminal nodes; default is 5
original.rf <- randomForest(paid.back ~ ., 
                            data = training, 
                            ntree = 50,
                            mtry=9,
                            importance=TRUE, 
                            na.action=na.omit)


##########################
# Random Forest in caret #
##########################

seeds <- as.vector(c(1:26), mode = "list")
seeds[[26]] <- 1

caret.rf <- train(training[, -28], training$paid.back, # 
                  method="rf",
                  ntree=50,
                  tuneGrid=data.frame(mtry=9),
                  importance=TRUE, 
                  na.action=na.omit,
                  trControl=trainControl(method="boot", seeds = seeds),
                  allowParallel=FALSE)

print(original.rf) 
print(caret.rf$finalModel) 

importance(original.rf)
importance(caret.rf$finalModel)

remove(seeds)


#################
# Flexing caret #
#################


mtryGrid <- expand.grid(mtry = seq(2,27,5))

set.seed(1)
seeds <- vector(mode = "list", length = 26) # length is = (nresampling)+1
for(i in 1:25) seeds[[i]]<- sample.int(n=1000, 6) # 6 is the number of tuning parameters (mtry possibilities)
seeds[[26]] <- 1 #for the last model
remove(i)

library(doParallel)
cl = makeCluster(4)
registerDoParallel(cl)

RF <- train(training[, -28], training$paid.back, 
            method="rf",
            ntree=200,
            metric="RMSE",
            importance=TRUE, 
            na.action=na.omit,
            trControl=trainControl(method="boot", number= 25, seeds=seeds),
            tuneGrid = mtryGrid,
            allowParallel=TRUE)

stopCluster(cl)
remove(cl)
registerDoSEQ()

# Examine results
print(RF$finalModel) 
RF
plot(RF, metric = "RMSE")
resampleHist(RF)


########################
# Generate predictions #
########################

# Original model
original.preds <- predict(object = original.rf, newdata = testing) # Predict the test data
results <- data.frame(actual = round(testing$paid.back, 2), original.preds = round(original.preds, 2))

# caret model
caret.preds <- predict(object = RF, newdata = testing) # Predict the test data
results <- data.frame(results, caret.preds = round(caret.preds, 2))

# Examine mean squared error in test data
results$original.residual <- results$actual - results$original.preds 
results$original.residual2 <- results$original.residual^2
mean(results$original.residual2)

results$caret.residual <- results$actual - results$caret.preds 
results$caret.residual2 <- results$caret.residual^2
mean(results$caret.residual2)