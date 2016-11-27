library(tm)
jobs.corpus <- Corpus(VectorSource(jobs))


###################
# Preprocess data #
###################

jobs.corpus <- tm_map(jobs.corpus, tolower)
as.character(jobs.corpus[[1]]) # examine the changes to job posting #1

jobs.corpus <- tm_map(jobs.corpus, removeNumbers)

jobs.corpus <- tm_map(jobs.corpus, removePunctuation)

for (j in seq(jobs.corpus))
{
    jobs.corpus[[j]] <- gsub("/", " ", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub(" c ", " c_plus_plus ", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("causal inference", "causal_inference", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("communication skills", "communication_skills", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("computer science", "computer_science", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("decision trees", "decision_tree", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("decision tree", "decision_tree", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("dimension reduction", "dimension_reduction", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("dimensionality reduction", "dimension_reduction", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("experimental design", "experimental_design", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("external data", "external_data", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("feature engineering", "feature_engineering", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("feature selection", "feature_selection", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("logistic regression", "logistic_regression", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("ml", "machine_learning", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("machine learning", "machine_learning", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("natural language processing", "nlp", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("neural networks", "neural_network", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("neural network", "neural_network", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("parallel processing", "parallel_processing", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("propensity modeling", "propensity_modeling", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub(" r ", " r_program ", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("random forests", "random_forest", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("random forest", "random_forest", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("supervised learning", "supervised_learning", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("svms", "svm", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("support vector machines", "svm", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("support vector machine", "svm", jobs.corpus[[j]])
    
    jobs.corpus[[j]] <- gsub("text mining", "text_mining", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("time series", "time_series", jobs.corpus[[j]])
    jobs.corpus[[j]] <- gsub("unsupervised learning", "unsupervised_learning", jobs.corpus[[j]])
    
}

remove(j)

jobs.corpus <- tm_map(jobs.corpus, stripWhitespace)
jobs.corpus <- tm_map(jobs.corpus, removeWords, stopwords("english")) # remove commonly-used words (e.g., "is", "what", etc.)

library(SnowballC)
jobs.corpus <- tm_map(jobs.corpus, stemDocument) # Removing common word endings (e.g., “ing”, “es”, “s”)

jobs.corpus <- tm_map(jobs.corpus, PlainTextDocument) # convert corpus to plain text doc for tdm

df <- TermDocumentMatrix(jobs.corpus)
df <- as.matrix(df) # convert TermDocumentMatrix object to a matrix object
df <- data.frame(df) # convert matrix to a data frame

names(df)[1:length(jobs)] <- paste("job", 1:length(jobs), sep="")

n_jobs <- ncol(df)
df[, 1:n_jobs] <- sapply(df[, 1:n_jobs], function(x) ifelse(x > 0, 1, 0))
df$percentage <- (rowSums(df)/n_jobs)*100
remove(n_jobs)

df <- df[order(-df$percentage),]

df$word <- rownames(df)
df <- subset(df, select=c("word", "percentage"))


############################
# Specify words to include #
############################

tools <- c("aws","c_plus_plus","cassandra","excel","hadoop","hive","java","kafka","mapreduce","mahout","matlab","nosql","oracle",
           "perl","pig","python", "r_program", "ruby","sas","scala","spark", "spss","stata","sql","sqlserver")
techniques <- c("classification","clustering","decision_tree","dimension_reduction",
                "feature_engineering", "feature_selection", "forecasting","logistic_regression",
                "machine_learning", "nlp","neural_network", "optimization",
                "parallel_processing","predictive","propensity_modeling", 
                "random_forest","regression","segmentation",
                "supervised_learning", "svm", "unsupervised_learning", "text_mining")
other <- c("communication_skills", "computer_science", "experimental_design", "external_data",
           "scalable","statistics","visualization")

df <- subset(df, word %in% tools | word %in% techniques | word %in% other)
remove(tools, techniques, other)


##################
# Clean up words #
##################

df$word[df$word=="c_plus_plus"] <- "C/C++"
df$word[df$word=="aws"] <- "Amazon Web Services"
df$word[df$word=="mapreduce"] <- "MapReduce"
df$word[df$word=="nlp"] <- "NLP"
df$word[df$word=="nosql"] <- "NoSQL"
df$word[df$word=="r_program"] <- "R"
df$word[df$word=="sas"] <- "SAS"
df$word[df$word=="spss"] <- "SPSS"
df$word[df$word=="sql"] <- "SQL"
df$word[df$word=="sqlserver"] <- "SQLServer"
df$word[df$word=="svm"] <- "support vector machines"
df$word[df$word=="visualization"] <- "data visualization"

df$word <- gsub("_", " ", df$word)


####################
# Create wordcloud #
####################

library(wordcloud)
library(RColorBrewer)
par(bg='white') # set plot background to white

# color palette options at http://www.sthda.com/sthda/RDoc/images/rcolorbrewer.png
wordcloud(words=df$word, freq=df$percentage, 
          scale=c(4,.5), # range of the word size
          random.order=FALSE, # plot words in decreasing freq value
          random.color=FALSE, # word color based upon freq value
          rot.per= 0, # proportion of words with 90 degree rotation
          colors=brewer.pal(n=8, name="Dark2")) # n=number of colors to use; name= name of color palette