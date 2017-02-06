
#######################################
# Aggregated financial impact by year #
#######################################

library(dplyr)

# Table 1
table.1 <- data %>% 
    group_by(year, payout) %>%
    summarise(Number_of_ACOs = n(), 
              Savings_relative_to_benchmark = sum(savings, na.rm=TRUE),
              Total_Payouts = sum(earned_shared_savings_payments_owe_losses3_4, na.rm=TRUE),
              Total_Savings = Savings_relative_to_benchmark - Total_Payouts)
print("Table 1")
table.1
cat(rep("\n",3))

library(WriteXLS) # Output table to Excel
testPerl(perl = "perl", verbose = TRUE)
WriteXLS(c("table.1"), 
         ExcelFileName = "SSP_Financial_Savings.xlsx", 
         SheetNames = c("SSP Financial Savings over Time"), 
         perl = "perl",
         verbose = FALSE,
         row.names = FALSE, col.names = TRUE,
         AdjWidth = TRUE, BoldHeaderRow = TRUE,
         FreezeRow = 1, FreezeCol = 1) 

remove(table.1)


####################################
# Change in savings rate over time #
####################################

# Scale predictors so intercepts are meaningful in the models
data$year_in_program_scaled <- data$year_in_program - 1
data$beneficiaries_scaled <- scale(data$total_assigned_beneficiaries, center = TRUE, scale = FALSE)
data$aco_11_scaled <- scale(data$aco_11, center = TRUE, scale = FALSE)

# confirm that the dependent variable is normally distributed
library(ggplot2)
library(scales)
options(scipen=999)
ggplot(data, aes_string(x="savings.rate")) + 
    geom_histogram() + scale_x_continuous(labels=percent) +
    theme(plot.title = element_text(hjust = 0.5)) + # center plot title
    ggtitle("savings.rate Frequency Distribution")

# one ACO had a savings.rate of -40% in their first year and then improved; another had a savings.rate of -30% in their second year
delete <- subset(data, savings.rate < -0.20)
delete1 <- subset(data, 
                  aco_num == delete[1,1], 
                  select=c("aco_name","year","year_in_program","total_benchmark_expenditures","total_expenditures",
                           "savings","savings.rate"))
delete2 <- subset(data, 
                  aco_num == delete[2,1], 
                  select=c("aco_name","year","year_in_program","total_benchmark_expenditures","total_expenditures",
                           "savings","savings.rate"))
remove(delete, delete1, delete2)

# we don't need to remove the 31 observations with no aco_num - 
# they'll automatically be excluded when the model is estimated
mlm.data <- subset(data, savings.rate > -0.20)

library(nlme)
model.0 <- lme(savings.rate ~ 1, # intercept-only model
               data=mlm.data, 
               random = ~ 1|aco_num, # random intercept
               na.action="na.omit")

# Function to calculate the interclass correlation
ICC.lme <- function(out) {
    varests <- as.numeric(VarCorr(out)[1:2])
    return(paste("ICC =", varests[1]/sum(varests)))
    
}
ICC.lme(model.0)

# year_in_program: random intercept model
model.1 <- lme(savings.rate ~ 1 + year_in_program_scaled, 
               data=mlm.data, 
               random = ~ 1|aco_num, # random intercept
               na.action="na.omit")
summary(model.1)

# year_in_program: random intercept model and random effect of year_in_program
model.2 <- lme(savings.rate ~ 1 + year_in_program_scaled, 
               data=mlm.data, 
               random = ~ year_in_program_scaled|aco_num, # random intercept AND random slope
               na.action="na.omit")
summary(model.2)

# test of impact of random slope; fixed effects must be the same in the models
anova(model.1, model.2)


# Add number of assigned beneficiaries
model.3 <- lme(savings.rate ~ 1 + year_in_program_scaled + beneficiaries_scaled, 
               data=mlm.data, 
               random = ~ year_in_program_scaled|aco_num, # random intercept AND random slope
               na.action="na.omit")
summary(model.3)

graph.data <- subset(mlm.data, ! is.na(mlm.data$aco_num))
full.plot <- ggplot() +
    # Graph separate regression lines for each ACO
    geom_smooth(data=graph.data, 
                aes(x = year_in_program, y = savings.rate, group = aco_num),
                method='lm', se=FALSE, colour="darkgrey", size = 0.2) +
    # Graph model-implied values for sample as a whole
    geom_smooth(data=graph.data, 
                aes(x = year_in_program, y = savings.rate),
                method='lm', se=FALSE, colour="black", size = 0.5) +
    # Plot formatting
    theme(plot.title = element_text(hjust = 0.5)) + # center plot title
    scale_y_continuous(labels=percent) +
    scale_x_continuous(breaks=c(1, 2, 3)) +
    xlab("Year in Shared Savings Program") +
    ylab("Savings Rate") +
    ggtitle("Savings Rate by Year-in-the-Program\n(for each ACO (grey lines) and sample as a whole (black line))") 
full.plot

zoom.plot <- full.plot + coord_cartesian(ylim=c(0, 0.015))
zoom.plot

remove(graph.data, full.plot, zoom.plot)
remove(mlm.data, model.0, model.1, model.2, model.3, ICC.lme)


#######################################
# Change in quality of care over time #
#######################################

library(ggplot2)

# QOC distributions
for (i in 1:length(qoc.items)) {
    
    column <- qoc.items[i]
    print(noquote(column))
    
    # subset to remove NA values before histogram
    hist.data <- data[!is.na(data[,column]),]
    
    # obtain recommended binwidth
    recommended.binwidth <- diff(range((hist.data[,column]))/30)
    
    # Frequency distributions
    title <- paste(column, "Frequency Distribution")
    hist.graph <- ggplot(hist.data, aes_string(x=column)) + geom_histogram(binwidth=recommended.binwidth) + ggtitle(title)
    print(hist.graph)
    remove(hist.data, title, recommended.binwidth, hist.graph)
    
    cat(rep("\n",1))
    
    remove(column)
    
}

remove(i)


# Transform skewed variables
data$aco_11_squared <- (data$aco_11)^2
data$aco_17_squared <- (data$aco_17)^2
data$aco_27_log <- log(data$aco_27)
data$aco_30_squared <- (data$aco_30)^2
data$aco_31_squared <- (data$aco_31)^2

qoc.items[[11]] <- "aco_11_squared"
qoc.items[[16]] <- "aco_17_squared"
qoc.items[[21]] <- "aco_27_log"
qoc.items[[23]] <- "aco_30_squared"
qoc.items[[24]] <- "aco_31_squared"
qoc.items


# Check intraclass correlations
ICC.lme <- function(out) {
    varests <- as.numeric(VarCorr(out)[1:2])
    return(paste("ICC =", varests[1]/sum(varests)))
}

library(nlme)

for (i in 1:length(qoc.items)) {
    
    column <- qoc.items[i]
    print(noquote(column))
    
    f <- as.formula(paste(column, "~ 1"))
    print(f)
    
    model.0 <- lme(f, # fixed effects formula
                   data=data, 
                   random = ~ 1|aco_num, # random intercept
                   na.action="na.omit")
    print(ICC.lme(model.0))
    remove(column, f, model.0)
    
    cat(rep("\n",1))
    
}

remove(i, ICC.lme)


# Multilevel Models
for (i in 1:length(qoc.items)) {
    
    tryCatch({
        
        column <- qoc.items[i]
        print(column)
        
        
        # customize formula for each QOC item
        f <- as.formula(paste(column, "~ 1 + year_in_program_scaled"))
        print(f)
        
        model.1 <- lme(f, # fixed effects formula
                       data=data, 
                       random = ~ 1|aco_num, # random intercept
                       na.action="na.omit")
        
        model.2 <- lme(f, # fixed effects formula
                       data=data, 
                       random = ~ year_in_program_scaled|aco_num, # random intercept and random slope
                       na.action="na.omit")
        
        # test of impact of random slope; fixed effects must be the same in the models
        anova.test <- anova(model.1, model.2)
        
        # if the random slope is significant, print the results from that model;
        # if not, print results from random intercept model.
        if (anova.test$p[2] < 0.05) {
            print("model.2 Results")
            print(summary(model.2))
        } else {
            print("model.1 Results")
            print(summary(model.1)) 
        }
        
        cat(rep("\n",3))
        remove(column, f, model.1, model.2, anova.test)
        
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    
}

remove(i)


# Run models for remaining items
qoc.items <- c("aco_4", "aco_6", "aco_21", "aco_28", "aco_31")

for (i in 1:length(qoc.items)) {
    
    column <- qoc.items[i]
    print(column)
    
    # customize formula for each QOC item
    f <- as.formula(paste(column, "~ 1 + year_in_program_scaled"))
    print(f)
    
    model.1 <- lme(f, # fixed effects formula
                   data=data, 
                   random = ~ 1|aco_num, # random intercept
                   na.action="na.omit")
    print(summary(model.1)) 
    
    cat(rep("\n",3))
    remove(column, f, model.1)
    
}

remove(i)
