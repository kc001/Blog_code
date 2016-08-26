# Import data
# https://archive.ics.uci.edu/ml/datasets/Online+Retail
library(XLConnect)
raw.data <- readWorksheet(loadWorkbook("Online Retail.xlsx"), sheet=1)

# Prepare data
data <- raw.data # the data takes a couple of minutes to import, so I keep raw.data as a backup
str(data)

length(unique(data$CustomerID))
sum(is.na(data$CustomerID))
data <- subset(data, !is.na(data$CustomerID))

range(data$InvoiceDate)
data <- subset(data, InvoiceDate >= "2010-12-09")
range(data$InvoiceDate)

table(data$Country)
data <- subset(data, Country == "United Kingdom")

length(unique(data$InvoiceNo))
length(unique(data$CustomerID))

# Identify returns
data$item.return <- grepl("C", data$InvoiceNo, fixed=TRUE) 
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)


#################################
# Create customer-level dataset #
#################################

customers <- as.data.frame(unique(data$CustomerID))
names(customers) <- "CustomerID"


###########
# Recency #
###########

data$recency <- as.Date("2011-12-10") - as.Date(data$InvoiceDate)

# remove returns so only consider the data of most recent *purchase*
temp <- subset(data, purchase.invoice == 1)

# Obtain # of days since most recent purchase
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)
remove(temp)

# Add recency to customer data
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE) 
remove(recency)

customers$recency <- as.numeric(customers$recency)


#############
# Frequency #
#############

customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) <- NULL

# Number of invoices/year (purchases only)
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

# Add # of purchase invoices to customers data
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE) 
remove(customer.invoices, annual.invoices)

range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0) 


###############################
# Monetary Value of Customers #
###############################

# Total spent on each item on an invoice
data$Amount <- data$Quantity * data$UnitPrice

# Aggregated total sales to customer
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)
remove(annual.sales)

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year
hist(customers$monetary)
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # reset negative numbers to zero
hist(customers$monetary)


##############
# 80/20 Rule #
##############

customers <- customers[order(-customers$monetary),]

# Apply Pareto Principle (80/20 Rule)
pareto.cutoff <- 0.8 * sum(customers$monetary)
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)

round(prop.table(table(customers$pareto)), 2)

remove(pareto.cutoff)

customers <- customers[order(customers$CustomerID),]


###################
# Preprocess data #
###################

# Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)

# Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)


##################
# Visualize data #
##################

library(ggplot2)
library(scales)

# Original scale
scatter.1 <- ggplot(customers, aes(x = frequency, y = monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency, shape = pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

# How many customers are represented by the two data points in the lower left-hand corner of the plot? 18
delete <- subset(customers, monetary.log < 0)
no.value.custs <- unique(delete$CustomerID)
delete2 <- subset(data, CustomerID %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerID, delete2$InvoiceDate),]
remove(delete, delete2, no.value.custs)

# Scaled variables
scatter.3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

remove(scatter.1, scatter.2, scatter.3)


#############################################
# Determine number of clusters / run kmeans #
#############################################


preprocessed <- customers[,9:11]
j <- 10 # specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

set.seed(1)
for (k in 1:j ) {
    
    print(k)
    
    # Run kmeans
    # nstart = number of initial configurations; the best one is used
    # $iter will return the iteration used for the final model
    output <- kmeans(preprocessed, centers = k, nstart = 20)
    
    # Add cluster membership to customers dataset
    var.name <- paste("cluster", k, sep="_")
    customers[,(var.name)] <- output$cluster
    customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
    
    # Graph clusters
    cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
    cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
    colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
    cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
    cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
    cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
    title <- paste("k-means Solution with", k, sep=" ")
    title <- paste(title, "Clusters", sep=" ")
    cluster_graph <- cluster_graph + ggtitle(title)
    print(cluster_graph)
    
    # Cluster centers in original metrics
    library(plyr)
    print(title)
    cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,  
                             monetary=round(median(monetary),2),  # use median b/c this is the raw, heavily-skewed data
                             frequency=round(median(frequency),1), 
                             recency=round(median(recency), 0))
    names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
    print(cluster_centers)
    cat("\n")
    cat("\n")
    
    # Collect model information
    models[k,("k")] <- k
    models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
    models[k,("betweenss")] <- output$betweenss
    models[k,("totss")] <- output$totss # betweenss + tot.withinss
    models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
    assign("models", models, envir = .GlobalEnv) 
    
    remove(output, var.name, cluster_graph, cluster_centers, title, colors)
    
}

remove(k)


library(ggplot2)
library(scales)

# Graph variance explained by number of clusters
r2_graph <- ggplot(models, aes(x = k, y = rsquared))
r2_graph <- r2_graph + geom_point() + geom_line()
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent)
r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j)
r2_graph <- r2_graph + xlab("k (Number of Clusters)")
r2_graph <- r2_graph + ylab("Variance Explained")
r2_graph

# Graph within sums of squares by number of clusters
# Look for a "bend" in the graph, as with a scree plot
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line()
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph

remove(j, r2_graph, ss_graph)


#########################################################
# Using NbClust metrics to determine number of clusters #
#########################################################

library(NbClust)
set.seed(1)
nc <- NbClust(preprocessed, min.nc=2, max.nc=7, method="kmeans")
table(nc$Best.n[1,])

nc$All.index # estimates for each number of clusters on 26 different metrics of model fit

barplot(table(nc$Best.n[1,]), 
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")

remove(preprocessed)


#######################
# Plot clusters in 3D #
#######################

colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')

library(car)
library(rgl)

scatter3d(x = customers$frequency.log, 
          y = customers$monetary.log,
          z = customers$recency.log, 
          groups = customers$cluster_5,
          xlab = "Frequency (Log-transformed)", 
          ylab = "Monetary Value (log-transformed)",
          zlab = "Recency (Log-transformed)",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          #     ellipsoid = TRUE, # to graph ellipses uses this command and set "surface = " to FALSE
          grid = TRUE,
          axis.col = c("black", "black", "black"))

remove(colors)