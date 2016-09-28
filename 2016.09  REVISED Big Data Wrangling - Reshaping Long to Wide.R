
options(scipen = 999)
n <- 100000  # Set the sample size. 
             # You can tinker with this if you want to test performance with different sample sizes. 

###########################
# Generate simulated data #
###########################

id <- seq(1,n, by=1)
female <- sample(0:1, n, replace=TRUE)
race <- sample(1:4, n, replace=TRUE)
prog <- sample(1:3, n, replace=TRUE)
schtyp <- sample(1:2, n, replace=TRUE)
ses <- sample(1:3, n, replace=TRUE)

master <- cbind(id,female,race,prog,schtyp,ses)
master <- as.data.frame(master)
remove(female, race, prog, schtyp, ses)

school.subject <- c("math","read","science","socst","write")
data.long <- merge(as.data.frame(id), as.data.frame(school.subject), all=TRUE)
remove(school.subject)
data.long <- merge(master, data.long, by=c("id"), all=TRUE)
data.long <- data.long[order(data.long$id, data.long$school.subject),]
rownames(data.long) <- NULL

s <- n*5
data.long$score <- sample(40:60, size=s, replace=TRUE)
remove(id, n, s)


################################################################
# Create a function from my previous subset-and-merge approach #
################################################################

# create the function
library(plyr)
subset.and.merge <- function(data.long) {
    
    wide.new <- subset(master, select=c("id"))
    row.names(wide.new) <- NULL
    
    repeating <- unique(data.long$school.subject)
    
    # Subset by school subject, rename, and recombine (wide)
    for (i in 1:length(repeating) ) {
        
        level <- repeating[i]
        data <- subset(data.long, school.subject==level, select=c(id, score))
        
        var.name <- paste(level, "score", sep=".")
        
        data <- rename(data, c("score" = var.name))
        
        wide.new <- merge(wide.new, data, by=c("id"), sort=TRUE, all=TRUE)
        
    }
    
    wide.new <- merge(master, wide.new, by=c("id"), sort=TRUE, all=TRUE)
    
    # save output to global environment
    assign("wide.new", wide.new, envir = .GlobalEnv) 
    
}

###########################################################
# Microbenchmark functions that reshape from long to wide #
###########################################################

library(microbenchmark)
library(reshape2)
library(tidyr) 

results <- microbenchmark(
    
    reshape = reshape(data.long, 
                           timevar = "school.subject",
                           idvar = c("id","female","prog","race","schtyp","ses"),
                           direction = "wide"),
    
    dcast = dcast(data.long, id + female + prog + race + schtyp + ses ~ school.subject, value.var="score"),
    
    subset.and.merge = subset.and.merge(data.long),
    
    spread = spread(data.long, key = school.subject, value = score),

    times=100L, # number of times to evaluate expression
    unit = "s" # print() output time in seconds; time var itself is still measured in nanoseconds 
    )

print(results) # summary stats (min, lower quartile, mean, median, etc. for each expression)
autoplot(results, log = TRUE)
boxplot(results, unit = "s", log = TRUE) # boxplot of output

library(ggplot2)
ggplot(data=results, aes(y = log(time), x = seq_along(time), colour=expr)) +
    geom_point() +
    scale_color_discrete(name ="Expressions\nEvaluated") +
    ylab("Nanoseconds (log)") +
    xlab("Microbenchmarking Trial") +
    ggtitle("Speed of Four Approaches to Reshaping\nA 100,000-Observation Dataset from Long Wide") +
    theme(axis.title = element_text(face="bold")) + # bold plot title
    theme(plot.title = element_text(face="bold")) # bold axis labels


###########################
# To obtain reshaped data #
###########################

library(tidyr) 
data.wide <- spread(data.long, key = school.subject, value = score)