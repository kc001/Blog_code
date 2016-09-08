# Set working directory to wherever you saved the data
setwd()

# Import file
loan <- read.csv("LoanStats3a.csv")

# Set missing data to NA
is.na(loan) <- loan == ""

# Create date variables that R recognizes as dates
loan$issue_d <- as.Date(paste('15', loan$issue_d), format='%d %b-%y')
loan$earliest_cr_line <- as.Date(paste('15', loan$earliest_cr_line), format='%d %b-%y')

# Identify loans that have already come to term
loan$term.months <- NA
loan$term.months[loan$term==" 36 months"] <- 36
loan$term.months[loan$term==" 60 months"] <- 60
# the "term" variable is redundant with the numerical term.months variable
loan$term <- NULL 

library(lubridate)
loan$maturity.date <- loan$issue_d + months(loan$term.months)
today <- Sys.Date()
loan$mature <- ifelse(loan$maturity.date < today, 1, 0)
loan$maturity.date <- NULL
remove(today)

# subset data to select only mature loans
loan <- subset(loan, mature==1)

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

# randomForest can only accommodate factor variables with 32 or fewer levels
# Remove factor vars with too many levels
too.many.levels <- function(x) {
    is.factor(x) == TRUE & length(levels(x)) > 32
}
delete <- lapply(loan, too.many.levels)
loan <- loan[, delete == FALSE]
remove(too.many.levels, delete)

# Calculate the percentage of loan paid back
# This is the outcome variable we will be looking to model in the training data,
# and predict in the test data
loan$paid.back <- (loan$funded_amnt - loan$out_prncp)/loan$funded_amnt
hist(loan$paid.back)
range(loan$paid.back, na.rm = TRUE)

# Remove accounts with missing paid.back status
loan <- subset(loan, ! is.na(loan$paid.back))

# Remove variables that provide additional outcome data about the loan
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


##################################
# Create train and test datasets #
##################################

library(dplyr)
training.per <- 0.75
training.n <- round((nrow(loan)*training.per), 0)
train <- sample_n(loan, training.n)
remove(training.n, training.per)
train.nums <- unique(train$member_id)

test <- subset(loan, !(member_id %in% train.nums))
remove(train.nums)
# predict() won't work for cases that are missing any of the predictor variables
test <- test[complete.cases(test),] 
row.names(test) <- NULL

# Remove ID number so randomForest doesn't try to use it as a predictor
train$member_id <- NULL
test$member_id <- NULL


##############################
# Random Forest - Regression #
##############################

library(randomForest)
rf.model <- randomForest(paid.back ~ .,
                         data = train,
                         ntree = 500,
                         type="regression",
                         importance=TRUE,
                         na.action=na.omit)

print(rf.model) # view results
importance <- as.data.frame(importance(rf.model)) # importance of each predictor
names(importance)[names(importance)=="%IncMSE"] <- "IncMSE"
importance <- importance[order(-importance$IncMSE),]

# loan issue date is the most important predictor
library(ggplot2)
library(scales)
issue_d_plot <- ggplot(loan, aes(x=issue_d, y=paid.back)) + geom_point()
issue_d_plot <- issue_d_plot + scale_y_continuous(labels=percent)
issue_d_plot <- issue_d_plot + ylab("Percentage of Loan Paid Back")
issue_d_plot
remove(issue_d_plot)

# Graph error rate as a function of number of decision trees using ggplot
plot.data <- as.data.frame(plot(rf.model))
colnames(plot.data) <- c("Error")
plot.data$trees <- as.numeric(rownames(plot.data))

options(scipen = 999)
library(ggplot2)
library(scales)
rf.plot <- ggplot(plot.data, aes(x=plot.data$trees, y=plot.data$Error)) + geom_line(colour="#000099")
rf.plot <- rf.plot + xlab("Number of Decision Trees")
rf.plot <- rf.plot + ylab("Mean Squared Error")
rf.plot <- rf.plot + ggtitle("Mean Squared Error by Number of Decision Trees")
rf.plot
remove(rf.plot, plot.data)

# Use the model to predict outcomes in new data
rf.model.preds <- predict(object = rf.model, newdata = test) # Predict the test data
results <- data.frame(actual = round(test$paid.back, 2), predicted = round(rf.model.preds, 2))
remove(rf.model.preds)

# Examine mean squared error in test data
results$residual <- results$actual - results$predicted
results$residual2 <- results$residual^2
mean(results$residual2)

# Identify correct predictions
results$correct.prediction <- ifelse(results$actual==results$predicted, 1, 0)
table(results$correct.prediction)