library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(data.table)
library(JOUSBoost)
library(glmnet)

df2000.final <- read.csv('df2000_final.csv')
df2000.final$top40 <- ifelse(df2000.final$Peak.Position <= 40, 1, 0)

# Text processing
text.sample <- df2000.final$lyrics
corpus1 <- VCorpus(VectorSource(text.sample))
corpus2 <- tm_map(corpus1, content_transformer(tolower)) # lowercase
corpus3 <- tm_map(corpus2, removeWords, stopwords("english")) # remove non-content words
corpus4 <- tm_map(corpus3, removePunctuation) # remove punctuations
corpus5 <- tm_map(corpus4, removeNumbers) # remove numbers
corpus6 <- tm_map(corpus5, stemDocument, lazy = TRUE) # words stem

# Process into Matrix and pull out words with at least 2% occurence
dtm <- DocumentTermMatrix(corpus6)
threshold <- 0.05 * length(corpus6)
threshold.words <- findFreqTerms(dtm, lowfreq=threshold)
length(threshold.words)

dtm.clean <- DocumentTermMatrix(corpus6, control = list(dictionary = threshold.words))  
dim(as.matrix(dtm.clean))
dtm.clean

# Process the final dataframe to be used for model 
lyrics.temp <- data.frame(df2000.final, as.matrix(dtm.clean))
names(lyrics.temp)[1:50]
lyrics.dataset <- lyrics.temp[, c(7:20, 22:43, 46:ncol(lyrics.temp))]
names(lyrics.dataset)[1:50]
dim(lyrics.dataset)

lyrics.dataset$Month <- as.numeric(lyrics.dataset$Month)
lyrics.dataset$top40 <- as.factor(lyrics.dataset$top40)
sapply(lyrics.dataset, typeof)


# Split into training and testing data
train.index <- sample(nrow(lyrics.dataset), floor(0.7*nrow(lyrics.dataset)), replace=FALSE)
lyrics.train <- lyrics.dataset[train.index,]
lyrics.test <- lyrics.dataset[-train.index,]
dim(lyrics.train)
dim(lyrics.test)

# LASSO

Y <- lyrics.train$top40
X <- as.matrix(lyrics.train[, -c(35, 36)]) 
names(lyrics.train)[1:50]
result.lasso <- cv.glmnet(X, Y, alpha=1, family="binomial", type.measure = "auc")
save(result.lasso, file="lasso.RData")

plot(result.lasso)
lasso.lambda.min <- result.lasso$lambda.min
lasso.lambda.min

predict.lasso <- predict(result.lasso, as.matrix(lyrics.test[, -c(35, 36)]), type = "class", s="lambda.min")
mean(lyrics.test$top40 != predict.lasso)

# GLM
beta.lasso <- coef(result.lasso, s="lambda.min") 
beta <- beta.lasso[which(beta.lasso !=0),] 
beta <- as.matrix(beta)
beta <- rownames(beta)

# BOOSTING

n.train <- length(lyrics.train)
n.test <- length(lyrics.test)

Y.train <- ifelse(lyrics.train$top40 == 0, -1, 1)
Y.test <- ifelse(lyrics.test$top40 == 0, -1, 1)

boost <- adaboost(as.matrix(lyrics.train[,-c(35,36)]), Y.train, tree_depth = 3, n_rounds = 1, verbose = FALSE, control = NULL)
yhat_test_ada <- predict(boost, lyrics.test[,-c(35, 36)])
yhat_train_ada <- predict(boost, lyrics.train[,-c(35, 36)])

save(yhat_test_ada, yhat_train_ada, file="boost.RData")

load("boost.RData")
test_err <- mean(Y.test != yhat_test_ada)
train_err <- mean(Y.train != yhat_train_ada)
test_err
train_err

boost$trees

## ----- PREVIOUS Classification

# Y <- df2000.seasons$top40
# X <- as.matrix(df2000.seasons[, c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 21)]) 
# X
# 
# result.lasso <- cv.glmnet(X, Y, alpha=1, family="binomial")
# plot(result.lasso)
# lasso.lambda.1se <- result.lasso$lambda.1se
# lasso.lambda.1se
# result.lasso$cvm
# 
# predict.lasso <- predict(result.lasso, as.matrix(data2.test[, -1]), type = "class", s="lambda.1se")
# mean(data2.test$rating != predict.lasso)

## Text Mining