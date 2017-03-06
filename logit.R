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
train <- as.data.frame(do.call("rbind", lapply(json_data, process_row)))

# Select useful data fields
select_field <- function(origin_data){
  
  origin_data$votes <- as.numeric(origin_data$requester_upvotes_minus_downvotes_at_request)
  origin_data$title <- origin_data$request_title
  origin_data$text <- origin_data$request_text
  origin_data$word_count_title <- sapply(gregexpr("\\W+", origin_data$title), length) + 1
  origin_data$word_count_text <- sapply(gregexpr("\\W+", origin_data$text), length) + 1
  origin_data$account_age <- as.numeric(origin_data$requester_account_age_in_days_at_request)
  origin_data$comments <- as.numeric(origin_data$requester_number_of_comments_at_request)
  origin_data$posts <- as.numeric(origin_data$requester_number_of_posts_at_request)
  
  return (origin_data[c('votes', 'title', 'text', 'word_count_title', 'word_count_text', 'comments', 'posts', 'account_age')])
}
outcome <- ifelse(train$requester_received_pizza == 'TRUE', 1, 0)
train <- select_field(train)
train$outcome <- outcome

text_mining <- function(text) {
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
  #dtm <- DocumentTermMatrix(text)
  #terms <- as.data.frame(dimnames(dtm)$Terms)
  #colnames(terms) <- c("keyword")
  #freq_term <- colSums(as.matrix(dtm))
  #terms$freq <- freq_term
  return(DocumentTermMatrix(text))
  #DocumentTermMatrix(text, control=list(wordLength=c(3,10), bounds=list(global=c(20,1800)))) #terms with 3-10 chars in 50-500
}

dtm <- text_mining(train$title)
dtm_cluster <- as.matrix(removeSparseTerms(dtm, 0.98))

distance <- dist(t(dtm_cluster), method="euclidean")
set.seed(4041)
tree <- hclust(distance,method="ward.D")
plot(tree)
rect.hclust(tree,k=5,border="blue")

title_cluster <- function(data){
  cluster1 <- dtm_cluster[,c("broke","colleg","student")]
  cluster2 <- dtm_cluster[,c("hungri")]
  cluster3 <- dtm_cluster[,c("food","help","money")]
  cluster4 <- dtm_cluster[,c("just","love","usa")] # just,love,usa is negative
  cluster5 <- dtm_cluster[,c("famili","friday","kid","last","make","pay","pleas","tonight","will")]
  
  cluster1 <- ifelse(rowSums(cluster1)>0,1,0)
  cluster2 <- ifelse(cluster2>0,1,0)
  cluster3 <- ifelse(rowSums(cluster3)>0,1,0)
  cluster4 <- ifelse(rowSums(cluster4)>0,1,0)
  cluster5 <- ifelse(rowSums(cluster5)>0,1,0)
  
  # Create a Score table #
  Score <- matrix(data = 0, nrow(data), 5)
  Score[,1] <- as.matrix(cluster1)
  Score[,2] <- as.matrix(cluster2)
  Score[,3] <- as.matrix(cluster3)
  Score[,4] <- as.matrix(cluster4)
  Score[,5] <- as.matrix(cluster5)
  # Name the Columns/Clusters #
  colnames(Score) <- c("Title_Cluster1", "Title_Cluster2", "Title_Cluster3", "Title_Cluster4", "Title_Cluster5")
  # Combine Score matrix to the original Data #
  
  return (cbind(data, Score))
  
}
train <- title_cluster(train)

dtm <- text_mining(train$text)
dtm_cluster <- as.matrix(removeSparseTerms(dtm, 0.90))

distance <- dist(t(dtm_cluster), method="euclidean")
set.seed(4041)
tree <- hclust(distance,method="ward.D")
plot(tree)
rect.hclust(tree,k=7,border="blue")

text_cluster <- function(data) {
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
  Score <- matrix(data = 0, nrow(data), 7)
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
  
  return(cbind(data, Score))
}
train <- text_cluster(train)
str(train)

# Build model
logit <- glm(outcome ~ votes + word_count_title + word_count_text + comments + posts + account_age + Title_Cluster1 + Title_Cluster2 + Title_Cluster3 + Title_Cluster4 + Title_Cluster5 + Cluster1 + Cluster2 + Cluster3 + Cluster4 + Cluster5 + Cluster6 + Cluster7, data = train, family = "binomial")
#logit <- glm(outcome ~ votes + word_count_text + comments + posts + Title_Cluster2 + Title_Cluster5 + Cluster2 + Cluster4, data = train, family = "binomial")
summary(logit)


json_data <- fromJSON(file = 'test.json')
# Convert JSON data to a dataframe
process_row <- function(row) unlist(lapply(row, process_cell))
test <- as.data.frame(do.call("rbind", lapply(json_data, process_row)))
results <- test["request_id"]

test <- select_field(test)

dtm <- text_mining(test$title)
dtm_cluster <- as.matrix(dtm)
test <- title_cluster(test)

dtm <- text_mining(test$text)
dtm_cluster <- as.matrix(dtm)
test <- text_cluster(test)

logit_predict <- predict(logit, test, type="response")
summary(logit_predict)
results$requester_received_pizza <- ifelse(logit_predict > 0.21440,1,0)
summary(results)

write.csv(results, file = "submission.csv", row.names = FALSE)
