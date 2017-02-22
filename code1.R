# set working directory and install packages first
rm(list=ls(all=TRUE)) # Clear all 

library(rjson)
json_data <- fromJSON(file = 'train.json')
# Convert JSON data to a dataframe
process_cell <- function(cell) {
  if (is.null(cell) || length(cell) == 0) {
    return(NA)
  }
  if (length(cell)>1) {
    return(paste(cell, collapse='; '))    
  }
  return(cell)
}
process_row <- function(row) unlist(lapply(row, process_cell))
origin_data <- as.data.frame(do.call("rbind", lapply(json_data, process_row)))

# Select useful data fields
origin_data$outcome <- ifelse(origin_data$requester_received_pizza == 'TRUE', 1, 0)
origin_data$votes <- as.numeric(origin_data$requester_upvotes_minus_downvotes_at_request)
origin_data$title <- origin_data$request_title
origin_data$text <- origin_data$request_text
origin_data$word_count_title <- sapply(gregexpr("\\W+", origin_data$title), length) + 1
origin_data$word_count_text <- sapply(gregexpr("\\W+", origin_data$text), length) + 1
origin_data$account_age <- as.numeric(origin_data$requester_account_age_in_days_at_request)
origin_data$comments <- as.numeric(origin_data$requester_number_of_comments_at_request)
origin_data$posts <- as.numeric(origin_data$requester_number_of_posts_at_request)

train <- origin_data[c('outcome', 'votes', 'title', 'text', 'word_count_title', 'word_count_text', 'comments', 'posts', 'account_age')]

# Oversample
#set.seed(4041)
#train1 = train[train$outcome==1,]
#train0 = train[train$outcome==0,]
#num_obs_0 <- nrow(train0)
#train0_sample <- sample(num_obs_0, 994)
#train0 <- train0[train0_sample,]
#train <- rbind(train1, train0)

text <- train$text
library(NLP) 
library(tm)
text <- Corpus(VectorSource(text))
text  <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
otherwords <-c("get", "pizza");
text <- tm_map(text, removeWords, otherwords)
# Remove numbers #
text <- tm_map(text, removeNumbers) 
# Remove puncutuations and symbols #
text <- tm_map(text, removePunctuation) 
# Remove extra white space #
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, PlainTextDocument)  # Remove common word endings ("es", "ed", "s", "ing")
text <- tm_map(text, stemDocument)

dtm <- DocumentTermMatrix(text, control=list(wordLength=c(3,10), bounds=list(global=c(20,1800)))) #terms with 3-10 chars in 50-500 app desc
new_dtm <- removeSparseTerms(dtm, 0.90) # Remove sparse terms with sparsity larger than 90%
dtm_cluster <- as.matrix(new_dtm)

distance <- dist(t(dtm_cluster), method="euclidean")
set.seed(4041)
tree <- hclust(distance,method="ward.D")
plot(tree)
rect.hclust(tree,k=7,border="blue")

cluster1 <- dtm_cluster[,c("eat","need","someon","much","friend","next","paid","even","tri","give","someth","anyth","tonight","today","anyon","appreci","new","broke","hungri")]
cluster2 <- dtm_cluster[,c("forward","pay","now","right","back","got","last","month","time","like","love","make","one","want","know","live","job","money")]
cluster3 <- dtm_cluster[,c("just")]
cluster4 <- dtm_cluster[,c("realli","thank","week","day","food")]
cluster5 <- dtm_cluster[,c("work")]
cluster6 <- dtm_cluster[,c("can","help")]
cluster7 <- dtm_cluster[,c("will")]

cluster1 <- ifelse(rowSums(cluster1)>0,1,0)
cluster2 <- ifelse(rowSums(cluster2)>0,1,0)
cluster3 <- ifelse(cluster3>0,1,0)
cluster4 <- ifelse(rowSums(cluster4)>0,1,0)
cluster5 <- ifelse(cluster5>0,1,0)
cluster6 <- ifelse(rowSums(cluster6)>0,1,0)
cluster7 <- ifelse(cluster7>0,1,0)

# Create a Score table #
Score <- matrix(data = 0, nrow(train), 7)
Score[,1] <- as.matrix(cluster1)
Score[,2] <- as.matrix(cluster2)
Score[,3] <- as.matrix(cluster3)
Score[,4] <- as.matrix(cluster4)
Score[,5] <- as.matrix(cluster5)
Score[,6] <- as.matrix(cluster6)
Score[,7] <- as.matrix(cluster7)
# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7")
# Combine Score matrix to the original Data #
train <- cbind(train, Score)
str(train)

# Build a model: decision tree, logistic regression, artificial neural network
#library(rpart)
#rpart_model <- rpart(outcome ~ votes + comments + posts + account_age + Cluster1 + Cluster2 + Cluster3 + Cluster4 + Cluster5 + Cluster6, data = train, method="class")
#logit <- glm(outcome ~ votes + word_count + comments + posts + account_age + Cluster1 + Cluster2 + Cluster3 + Cluster4 + Cluster5 + Cluster6 + Cluster7, data = train, family = "binomial")
library(neuralnet)
train$n_votes <- (train$votes - min(train$votes))/(max(train$votes)-min(train$votes))
train$n_word_count_title <- (train$word_count_title - min(train$word_count_title))/(max(train$word_count_title)-min(train$word_count_title))
train$n_word_count_text <- (train$word_count_text - min(train$word_count_text))/(max(train$word_count_text)-min(train$word_count_text))
train$n_comments <- (train$comments - min(train$comments))/(max(train$comments)-min(train$comments))
train$n_posts <- (train$posts - min(train$posts))/(max(train$posts)-min(train$posts))
train$n_account_age <- (train$account_age - min(train$account_age))/(max(train$account_age)-min(train$account_age))
set.seed(4041)
ann = neuralnet(data = train, formula = outcome ~ n_votes + n_word_count_title + n_word_count_text + n_comments + n_posts + Cluster1 + Cluster2 + Cluster3 + Cluster4 + Cluster5 + Cluster6 + Cluster7, hidden=3, threshold = 0.01)
plot(ann)


json_data <- fromJSON(file = 'test.json')
# Convert JSON data to a dataframe
process_cell <- function(cell) {
  if (is.null(cell) || length(cell) == 0) {
    return(NA)
  }
  if (length(cell)>1) {
    return(paste(cell, collapse='; '))    
  }
  return(cell)
}
process_row <- function(row) unlist(lapply(row, process_cell))
test <- as.data.frame(do.call("rbind", lapply(json_data, process_row)))
test$votes <- as.numeric(test$requester_upvotes_minus_downvotes_at_request)
test$title <- test$request_title
test$text <- test$request_text
test$word_count_title <- sapply(gregexpr("\\W+", test$title), length) + 1
test$word_count_text <- sapply(gregexpr("\\W+", test$text), length) + 1
test$account_age <- as.numeric(test$requester_account_age_in_days_at_request)
test$comments <- as.numeric(test$requester_number_of_comments_at_request)
test$posts <- as.numeric(test$requester_number_of_posts_at_request)

text <- test$text

text <- Corpus(VectorSource(text))
text  <- tm_map(text, content_transformer(tolower))
text <- tm_map(text, removeWords, stopwords("english"))
otherwords <-c("get", "pizza");
text <- tm_map(text, removeWords, otherwords)
# Remove numbers #
text <- tm_map(text, removeNumbers) 
# Remove puncutuations and symbols #
text <- tm_map(text, removePunctuation) 
# Remove extra white space #
text <- tm_map(text, stripWhitespace)
text <- tm_map(text, PlainTextDocument)  # Remove common word endings ("es", "ed", "s", "ing")
text <- tm_map(text, stemDocument)

dtm <- DocumentTermMatrix(text)
dtm_cluster <- as.matrix(dtm)

cluster1 <- dtm_cluster[,c("eat","need","someon","much","friend","next","paid","even","tri","give","someth","anyth","tonight","today","anyon","appreci","new","broke","hungri")]
cluster2 <- dtm_cluster[,c("forward","pay","now","right","back","got","last","month","time","like","love","make","one","want","know","live","job","money")]
cluster3 <- dtm_cluster[,c("just")]
cluster4 <- dtm_cluster[,c("realli","thank","week","day","food")]
cluster5 <- dtm_cluster[,c("work")]
cluster6 <- dtm_cluster[,c("can","help")]
cluster7 <- dtm_cluster[,c("will")]

cluster1 <- ifelse(rowSums(cluster1)>0,1,0)
cluster2 <- ifelse(rowSums(cluster2)>0,1,0)
cluster3 <- ifelse(cluster3>0,1,0)
cluster4 <- ifelse(rowSums(cluster4)>0,1,0)
cluster5 <- ifelse(cluster5>0,1,0)
cluster6 <- ifelse(rowSums(cluster6)>0,1,0)
cluster7 <- ifelse(cluster7>0,1,0)

# Create a Score table #
Score <- matrix(data = 0, nrow(test), 7)
Score[,1] <- as.matrix(cluster1)
Score[,2] <- as.matrix(cluster2)
Score[,3] <- as.matrix(cluster3)
Score[,4] <- as.matrix(cluster4)
Score[,5] <- as.matrix(cluster5)
Score[,6] <- as.matrix(cluster6)
Score[,7] <- as.matrix(cluster7)
# Name the Columns/Clusters #
colnames(Score) <- c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7")
# Combine Score matrix to the original Data #
test <- cbind(test, Score)

results <- test["request_id"]

#results$requester_received_pizza <- predict(rpart_model, test, type="class")

#logit_predict <- predict(logit, train, type="response")
#results$requester_received_pizza <- ifelse(logit_predict > 0.5,1,0)

test$n_votes <- (test$votes - min(test$votes))/(max(test$votes)-min(test$votes))
test$n_word_count_title <- (test$word_count_title - min(test$word_count_title))/(max(test$word_count_title)-min(test$word_count_title))
test$n_word_count_text <- (test$word_count_text - min(test$word_count_text))/(max(test$word_count_text)-min(test$word_count_text))
test$n_comments <- (test$comments - min(test$comments))/(max(test$comments)-min(test$comments))
test$n_posts <- (test$posts - min(test$posts))/(max(test$posts)-min(test$posts))
test$n_account_age <- (test$account_age - min(test$account_age))/(max(test$account_age)-min(test$account_age))
test <- test[,c("n_votes","n_word_count_title","n_word_count_text","n_comments","n_posts","Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5", "Cluster6", "Cluster7")]
ann_predict <- compute(ann, test)
results$requester_received_pizza <- ifelse(ann_predict$net.result > 0.2052,1,0)


write.csv(results, file = "submission.csv", row.names = FALSE)
