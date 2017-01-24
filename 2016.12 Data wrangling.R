library(RSocrata)

###########################
# Annual performance data #
###########################

# https://data.cms.gov/ACO/Medicare-Shared-Savings-Program-Accountable-Care-O/yuq5-65xt
data.2013 <- read.socrata("https://data.cms.gov/resource/h46j-zm7v.json")

# https://data.cms.gov/ACO/Medicare-Shared-Savings-Program-Accountable-Care-O/ucce-hhpu
data.2014 <- read.socrata("https://data.cms.gov/resource/kcsi-wmjs.json")

# https://data.cms.gov/ACO/Medicare-Shared-Savings-Program-Accountable-Care-O/x8va-z7cu
data.2015 <- read.socrata("https://data.cms.gov/resource/rmpx-bjq8.json")


#################################################################
# ACO public-use files (PUF) for matching ACO data across years #
#################################################################

# Note that (as of this posting), the PUF files are only available for 2013 and 2015.

# https://data.cms.gov/Special-Programs-Initiatives-Accountable-Care-Orga/2013-Shared-Savings-Program-Accountable-Care-Organ/faep-t7cf
puf.2013 <- read.socrata("https://data.cms.gov/resource/qjhc-b5f2.json")

# https://data.cms.gov/Special-Programs-Initiatives-Accountable-Care-Orga/2015-Shared-Savings-Program-SSP-Accountable-Care-O/7rrf-3gxr
puf.2015 <- read.socrata("https://data.cms.gov/resource/rgk7-vd6w.json")


##########################
# CREATE ACO MASTER FILE #
##########################

# 2013 PUF data
temp.puf.2013 <- subset(puf.2013, select=c("aco_name", "aco_num", "start_date"))
temp.puf.2013$aco_name <- toupper(temp.puf.2013$aco_name) # Clean up ACO names
temp.puf.2013$aco_name <- gsub(", LLC.", "", temp.puf.2013$aco_name)
temp.puf.2013$aco_name <- gsub(", LLC", "", temp.puf.2013$aco_name)
temp.puf.2013$aco_name <- gsub(" LLC", "", temp.puf.2013$aco_name)
temp.puf.2013$aco_name <- trimws(temp.puf.2013$aco_name) # trim any leading or trailing white space
colnames(temp.puf.2013)[colnames(temp.puf.2013)=="start_date"] <- "agreement_start_date" # use results data name for this variable
temp.puf.2013 <- subset(temp.puf.2013, select=c("aco_name", "aco_num", "agreement_start_date")) # PUF data: final for merge
temp.puf.2013 <- temp.puf.2013[!duplicated(temp.puf.2013), ] # Remove duplicate rows
row.names(temp.puf.2013) <- NULL

# 2013 Results data
temp.2013 <- subset(data.2013, select=c("aco_name_lbn_or_dba_if_applicable", "agreement_start_date"))
temp.2013$aco_name <- toupper(temp.2013$aco_name_lbn_or_dba_if_applicable) # Clean up ACO names
temp.2013$aco_name <- gsub(", LLC.", "", temp.2013$aco_name)
temp.2013$aco_name <- gsub(", LLC", "", temp.2013$aco_name)
temp.2013$aco_name <- gsub(" LLC", "", temp.2013$aco_name)
temp.2013$aco_name <- trimws(temp.2013$aco_name)
temp.2013 <- subset(temp.2013, select=c("aco_name", "agreement_start_date")) # final data for merge
temp.2013 <- temp.2013[!duplicated(temp.2013), ] # Remove duplicate rows
row.names(temp.2013) <- NULL

# Merge 2012-2013 data sources - produces a list of 267 ACOs (vs. 220 in each separate file)
master <- merge(temp.2013, temp.puf.2013, by=c("aco_name", "agreement_start_date"), all=TRUE)
colnames(master)[colnames(master)=="aco_name"] <- "aco_name_2013"
remove(temp.2013, temp.puf.2013)


# 2015 PUF data
temp.puf.2015 <- subset(puf.2015, select=c("aco_name", "aco_num", "start_date"))
temp.puf.2015$aco_name <- toupper(temp.puf.2015$aco_name) # Clean up ACO names
temp.puf.2015$aco_name <- gsub(", LLC.", "", temp.puf.2015$aco_name)
temp.puf.2015$aco_name <- gsub(", LLC", "", temp.puf.2015$aco_name)
temp.puf.2015$aco_name <- gsub(" LLC", "", temp.puf.2015$aco_name)
temp.puf.2015$aco_name <- trimws(temp.puf.2015$aco_name) # trim any leading or trailing white space
colnames(temp.puf.2015)[colnames(temp.puf.2015)=="start_date"] <- "agreement_start_date" # use performance data name for this variable
temp.puf.2015 <- subset(temp.puf.2015, select=c("aco_name", "aco_num", "agreement_start_date")) # Org data: final for merge
temp.puf.2015 <- temp.puf.2015[!duplicated(temp.puf.2015), ] # Remove duplicate rows
row.names(temp.puf.2015) <- NULL

# Quality control check: 2015 Results data
temp.2015 <- subset(data.2015, select=c("aco_name", "current_start_date", "aco_num"))
temp.2015$aco_name <- toupper(temp.2015$aco_name) # Clean up ACO names
temp.2015$aco_name <- gsub(", LLC.", "", temp.2015$aco_name)
temp.2015$aco_name <- gsub(", LLC", "", temp.2015$aco_name)
temp.2015$aco_name <- gsub(" LLC", "", temp.2015$aco_name)
temp.2015$aco_name <- trimws(temp.2015$aco_name)
colnames(temp.2015)[colnames(temp.2015)=="current_start_date"] <- "agreement_start_date"
colnames(temp.2015)[colnames(temp.2015)=="aco_num"] <- "aco_num_in_2015_results"
temp.2015 <- subset(temp.2015, select=c("aco_name", "agreement_start_date", "aco_num_in_2015_results")) # final data for merge
temp.2015 <- temp.2015[!duplicated(temp.2015), ] # Remove duplicate rows
row.names(temp.2015) <- NULL

# Quality control check: Merge 2015 data sources. Produces a list of 392 ACOs, exactly as it should be.
reconcile.2015 <- merge(temp.puf.2015, temp.2015, by=c("aco_name", "agreement_start_date"), all=TRUE)
sum(is.na(reconcile.2015$aco_num)) # no missing aco_nums

# Quality control check: The aco_nums in the PUF all match the aco_nums in the results file, when merging by name.
reconcile.2015$test <- ifelse(reconcile.2015$aco_num == reconcile.2015$aco_num_in_2015_results, 1, 0)
table(reconcile.2015$test)
remove(temp.2015, reconcile.2015)


# Merge 2015 ACO information with 2013 master
colnames(temp.puf.2015)[colnames(temp.puf.2015)=="aco_name"] <- "aco_name_2015"
master <- merge(master, temp.puf.2015, by=c("aco_num","agreement_start_date"), all=TRUE)
remove(temp.puf.2015)

remove(puf.2013, puf.2015)


###############################
# Prepare annual results data #
###############################

# Add year to datasets 
data.2013$year <- 2013
data.2014$year <- 2014
data.2015$year <- 2015


###############################################
# Make variable names consistent across years #
###############################################

# 2013 data
colnames(data.2013)[colnames(data.2013)=="aco_name_lbn_or_dba_if_applicable"] <- "aco_name" # use 2015 name for this variable

# 2014 data
colnames(data.2014)[colnames(data.2014)=="aco_doing_business_as_dba_or_legal_business_name_lbn"] <- "aco_name" # 2015 name for this var
colnames(data.2014)[colnames(data.2014)=="total_benchmark_minus_assigned_beneficiary_expenditures_as_of_total_benchmark"] <- 
    "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark"

# 2015 data
# Clean up variable names to combine datasets
colnames(data.2015)[colnames(data.2015)=="initial_start_date"] <- "agreement_start_date"
colnames(data.2015)[colnames(data.2015)=="total_benchmark_minus_assigned_beneficiary_expenditures"] <- 
    "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures"
colnames(data.2015)[colnames(data.2015)=="total_benchmark_minus_assigned_beneficiary_expenditures_as_of_total_benchmark"] <- 
    "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark"

# Add the underscore ("_") to all the 2015 quality-of-care variables so they match the 2012-2013 and 2014 names
qoc.2015 <- names(data.2015)[1:33]
qoc.2015 <- gsub("aco", "aco_", qoc.2015)
colnames(data.2015)[1:33] <- qoc.2015
remove(qoc.2015)


# Identify the common variables across datasets
names.2013 <- names(data.2013)
names.2014 <- names(data.2014)
names.2015 <- names(data.2015)

# This gives us the variable names that appear in all 3 datasets
common.vars <- Reduce(intersect, list(names.2013, names.2014, names.2015))
remove(names.2013, names.2014, names.2015)
common.vars

# Use the common.vars to select data for rbind and put all vars in the same order
subsetted.2013 <- subset(data.2013, select=common.vars)
subsetted.2014 <- subset(data.2014, select=common.vars)
subsetted.2015 <- subset(data.2015, select=common.vars)
remove(common.vars, data.2013, data.2014, data.2015)

# Combine datasets 
data <- rbind(subsetted.2013, subsetted.2014, subsetted.2015)
remove(subsetted.2013, subsetted.2014, subsetted.2015)


#########################
# Prepare combined data #
#########################

# Create year_in_program variable
data$start.year <- as.POSIXlt(data$agreement_start_date)
data$start.year <- data$start.year$year+1900
data$start.year <- ifelse(data$start.year==2012, 2013, data$start.year)
data$year_in_program <- (data$year - data$start.year) + 1

# Re-order variables for ease of viewing
data <- subset(data, select=c("aco_name", "agreement_start_date", "year", "year_in_program","total_assigned_beneficiaries",
                              "total_benchmark_expenditures", "total_expenditures",
                              "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures",
                              "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark",
                              "generated_savings_losses1_2", "earned_shared_savings_payments_owe_losses3_4",
                              "aco_1","aco_2","aco_3","aco_4","aco_5","aco_6","aco_7","aco_8","aco_9",
                              "aco_10", "aco_11","aco_13","aco_14","aco_15","aco_16","aco_17","aco_18","aco_19",
                              "aco_20","aco_21","aco_27","aco_28",
                              "aco_30","aco_31","aco_33"))


##############################################
# Add the ACO unique identifiers to the data #
##############################################

# Step 1: Clean up ACO name for merging
data$aco_name <- toupper(data$aco_name) # Clean up ACO names
data$aco_name <- gsub(", LLC.", "", data$aco_name)
data$aco_name <- gsub(", LLC", "", data$aco_name)
data$aco_name <- gsub(" LLC", "", data$aco_name)
data$aco_name <- trimws(data$aco_name) # trim any leading or trailing white space

# Step 2: Merge in aco_num based upon 2013 name, only if it matches
colnames(data)[colnames(data)=="aco_name"] <- "aco_name_2013"
master.temp <- subset(master, select=c("aco_num", "aco_name_2013", "agreement_start_date"))
data <- merge(master.temp, data, by=c("aco_name_2013", "agreement_start_date"), all.y = TRUE)

# Step 3: Merge in aco_num based upon 2015 name, only if it matches
colnames(data)[colnames(data)=="aco_name_2013"] <- "aco_name_2015"
master.temp <- subset(master, select=c("aco_num", "aco_name_2015", "agreement_start_date"))
data <- merge(master.temp, data, by=c("aco_name_2015", "agreement_start_date"), all.y = TRUE, suffixes = c("_2015","_2013"))
remove(master.temp)
colnames(data)[colnames(data)=="aco_name_2015"] <- "aco_name"

# Now have two aco_num columns, based upon whether the aco_num was merged in using the 2013 or 2015 name for the business.
# When the name stayed consistent in both years, both columns will be populated.

# Step 4: Confirm that when both columns are populated for a business, the results match. They do.
test <- subset(data, ! is.na(data$aco_num_2013) & ! is.na(data$aco_num_2015))
test$test <- ifelse(test$aco_num_2013 == test$aco_num_2015, 1, 0)
table(test$test)
remove(test)

# Step 5: Re-create aco_num field using data from *either* aco_num_2013 or aco_num_2015, depending upon what's populated in each row
data$aco_num <- ifelse(is.na(data$aco_num_2015), data$aco_num_2013, data$aco_num_2015)

# Step 6: Manually check rows with missing aco_num data.
# If the preceding or following entry have a similar name, the same start date, and the year-in-the-program 
# suggest that these are subsequent years for the same ACO, manually impute aco_num data.
delete <- subset(data,
                 select=c("aco_name", "aco_num", "aco_num_2013","aco_num_2015", "agreement_start_date", "year", "year_in_program"))
delete <- delete[order(delete$aco_name, delete$agreement_start_date, delete$year),]
rownames(delete) <- NULL

data$aco_num[data$aco_name=="HCP ACO CA"] <- "A71307"
data$aco_num[data$aco_name=="JFK POPULATION HEALTH COMPANY"] <- "A06665"
data$aco_num[data$aco_name=="KCMPA"] <- "A05696"
remove(delete)

# Step 7: Double-check that no ACO has more than one entry for each year_in_program
library(dplyr)
aco_table <- data %>% # count rows in data for each ACO and each year
    group_by(aco_num, year_in_program) %>%
    summarise(Number_of_years_of_data = n())

library(tidyr) 
aco_table <- spread(aco_table, key = year_in_program, value = Number_of_years_of_data) # transpose wide
aco_table <- subset(aco_table, ! is.na(aco_table$aco_num)) # remove row with aco_num = NA
lapply(aco_table, function(x) { range(x, na.rm=TRUE) }) # check that no aco_num has more than 1 entry for each year_in_program
remove(aco_table)

# Clean up data
data$aco_num_2013 <- NULL
data$aco_num_2015 <- NULL

data <- subset(data, select=c(37, 1:36))
data <- data[order(data$aco_num, data$year),]


##################################################
# Make sure numerical data is recognized as such #
##################################################

# quality-of-care items should all be numerical
qoc.items <- names(data)[13:37]

for (i in 1:length(qoc.items)) {
    
    column <- qoc.items[i]
    print(column)
    
    # Make numeric
    data[, column] <- as.numeric(data[, column])
    
    # Look at variable range
    print(range(data[, column], na.rm=TRUE))
    cat("\n")
    remove(column)
    
}

remove(i)

data$total_assigned_beneficiaries <- as.numeric(data$total_assigned_beneficiaries)

# total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark
data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark <- 
    gsub("%", "", data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark)
data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark <- 
    as.numeric(data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark)

# earned_shared_savings_payments_owe_losses3_4
data$earned_shared_savings_payments_owe_losses3_4 <- gsub(",", "", data$earned_shared_savings_payments_owe_losses3_4)
data$earned_shared_savings_payments_owe_losses3_4 <- gsub(" ", "", data$earned_shared_savings_payments_owe_losses3_4)
data$earned_shared_savings_payments_owe_losses3_4 <- gsub("$", "", data$earned_shared_savings_payments_owe_losses3_4, fixed = TRUE)
data$earned_shared_savings_payments_owe_losses3_4 <- as.numeric(data$earned_shared_savings_payments_owe_losses3_4)
quantile(data$earned_shared_savings_payments_owe_losses3_4, probs = seq(0, 1, 0.10), na.rm = TRUE, names = TRUE, type = 7)


#############################
# Double-check some numbers #
#############################

# differences between my calculation and total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures
# are all from $-1 to $1; probably due to dropped cents from data
data$savings <- data$total_benchmark_expenditures - data$total_expenditures
range(data$savings - data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures)

# double-check total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark field
# SO GLAD I DID THIS!!
# In 2012-2013 and 2014, this value was a proportion (scaled 0-1) 
# In 2015, it was a percentage (scaled 0-100)
data$savings.prop <- data$savings/data$total_benchmark_expenditures
range(data$savings.prop - data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark)

delete <- subset(data, 
                 select=c("year","total_benchmark_expenditures","savings","savings.prop", 
                          "total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark"))
remove(delete)

# Use my "savings.prop" variable for consistency going forward
data$total_benchmark_expenditures_minus_total_assigned_beneficiary_expenditures_as_of_total_benchmark <- NULL

# Confirm that a given quality-of-care item was measured on the same scale across all three years of data
# Delightfully, the scaling for QOC variables looks to be consistent across years.
temp.2013 <- subset(data, year==2013)
temp.2014 <- subset(data, year==2014)
temp.2015 <- subset(data, year==2015)

for (i in 1:length(qoc.items)) {
    
    qoc_var <- qoc.items[i]
    print(qoc_var)
    print("2013 data")
    print(quantile(temp.2013[, qoc_var], probs = seq(0, 1, 0.10), na.rm = TRUE,names = TRUE, type = 7))
    print("2014 data")
    print(quantile(temp.2014[, qoc_var], probs = seq(0, 1, 0.10), na.rm = TRUE,names = TRUE, type = 7))
    print("2015 data")
    print(quantile(temp.2015[, qoc_var], probs = seq(0, 1, 0.10), na.rm = TRUE,names = TRUE, type = 7))
    
    cat("\n")
    remove(qoc_var)
    
}

remove(i, temp.2013, temp.2014, temp.2015)

# Create a variable that reflects Medicare's savings in each year with each ACO
# (= the amount under benchmark minus the ACO's earned savings)
data$medicare.savings <- data$savings - data$earned_shared_savings_payments_owe_losses3_4
data$medicare.savings <- ifelse(is.na(data$medicare.savings), data$savings, data$medicare.savings)
