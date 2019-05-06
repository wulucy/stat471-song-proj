df2000_grouped <- read.csv('df2000_grouped_morefactors.csv')
df2000_extra <- read.csv('df2000_withgenre_withyear.csv')

# Linear regression - AZ
lm_df <- df2000_extra %>%
  select(-artist, -song, -WeekID, -Weeks.on.Chart, -X.1, -X)

lm_df$Year <- as.factor(lm_df$Year)

summary(lm_df)

# Remove the columns that have no or very few observations
lm_df_1 <- lm_df %>%
  select(-instrumental, -bluegrass, -jazz, -classical, -broadway, -opera, -hip.hop)

# Add transformed variable to new data frame
lm_df_1$Peak.Transformed <- log(101 - lm_df$Peak.Position)
lm_df_1$Peak.Position <- NULL

set.seed(22)
train_indices <- sample(nrow(lm_df_1), nrow(lm_df_1) * 0.7, replace=FALSE)
train_df <- lm_df_1[train_indices,]
test_df <- lm_df_1[-train_indices,]

# Consider transformations
fit.linear <- lm(Peak.Position ~., lm_df)
fit.log <- lm(log(Peak.Position) ~. , lm_df)
fit.reverse <- lm(log(101- Peak.Position) ~., lm_df)
fit.square <- lm(Peak.Position^2 ~. , lm_df)

# Normal Q-Q Plots
par(mfrow = c(2,2))
plot(fit.linear, 2, main = "No Transformation")
plot(fit.log, 2, main = "Log Transformation")
plot(fit.reverse, 2, main = "Log+Reverse Transformation")
plot(fit.square, 2, main = "Square Transformation")

# Histograms of transformed distribution
par(mfrow = c(2,2))
hist(lm_df$Peak.Position)
hist(I(log(lm_df$Peak.Position)))
hist(I(log(101 - lm_df$Peak.Position)))
hist(I(lm_df$Peak.Position^2))


library(leaps)

# Backwards model selection with regsubsets (don't include Year)
fit.exh <- regsubsets(Peak.Transformed ~ . -Year, train_df, nvmax=25, method="backward")
f.e <- summary(fit.exh)
data.frame(variables = (1:length(f.e$rsq)),
           r_squared = f.e$rsq,
           rss = f.e$rss,
           bic = f.e$bic,
           cp = f.e$cp)

# Model with 11 variables minimized cp. Remove variables until all are significant
fit.backwards <- lm(Peak.Transformed ~ danceability + duration_ms + instrumentalness + valence +
                      artist.pop + trap + indie + country + folk + rock + funk, train_df)
summary(fit.backwards)
fit.backwards.1 <- lm(Peak.Transformed ~ danceability + duration_ms + valence +
                        artist.pop + trap + indie + country + folk + rock + funk, lm_df_1)
fit.backwards.2 <- lm(Peak.Transformed ~ danceability + duration_ms + valence +
                        artist.pop + trap + country + folk + rock + funk, lm_df_1)
summary(fit.backwards.2)

# Our lm final model
fit.final <- fit.backwards.2 

# Predicted values
yhat.lm.train <- 101 - exp(predict(fit.final, train_df))
range(yhat.lm.train)

mse.lm.train <- mean((lm_df[train_indices,]$Peak.Position-yhat.lm.train)^2)

yhat.lm.test <- 101 - exp(predict(fit.final, test_df))
range(yhat.lm.test)
mse.lm.test <- mean((lm_df[-train_indices,]$Peak.Position-yhat.lm.test)^2)


# Regression trees
library(rpart)
library(party)
library(rattle)		
library(rpart.plot)	


# Add Peak.Position back in
lm_df_2 <- lm_df_1
lm_df_2$Peak.Position <- lm_df$Peak.Position
lm_df_2$Peak.Transformed <- NULL
train_df_2 <- lm_df_2[train_indices,]
test_df_2 <- lm_df_2[-train_indices,]

tree.full <- rpart(Peak.Position ~ ., train_df_2)

par(mfrow = c(1,1))
fancyRpartPlot(tree.full)
yhat.tree.train <- predict(tree.full, train_df_2)
mse.tree.train <- mean((train_df_2$Peak.Position-yhat.tree.train)^2)
yhat.tree.test <- predict(tree.full, test_df_2)
mse.tree.test <- mean((test_df_2$Peak.Position-yhat.tree.test)^2)

# Random Forest
library(randomForest)

fit.rf <- randomForest(Peak.Position ~., train_df_2, mtry=10, ntree=500) 

par(mfrow=c(1,1))

plot(fit.rf, col="blue", pch=16, type="p", main="default plot")

# stabilizes around ntree = 200, now choose mtry

colnames(train_df)
rf.error.p <- 1:15

for (p in 1:15) {
  fit.rf <- randomForest(Peak.Position ~ ., train_df_2, mtry=p, ntree=200) 
  rf.error.p[p] <- fit.rf$mse[200] 
}

rf.error.p 

plot(1:15, rf.error.p[1:15], pch=16, xlab="mtry",
     ylab="OOB mse of mtry") 
lines(1:15, rf.error.p[1:15])

# choose mtry = 10 for final rf
fit.rf.final <- randomForest(Peak.Position ~ ., train_df_2, mtry=10, ntree=200) 

# Testing errors
plot(fit.rf.final$mse, xlab="number of trees", col="blue", ylab="ave mse up to i many trees using OOB predicted", pch=16)
fit.rf.final$mse[200]

yhat.rf.train <- predict(fit.rf.final, train_df_2)
mse.rf.train <- mean((train_df_2$Peak.Position-yhat.rf.train)^2)

yhat.rf.test <- predict(fit.rf.final, test_df_2)
mse.rf.test <- mean((test_df_2$Peak.Position-yhat.rf.test)^2)

hist(yhat.rf.train)
range(yhat.rf.train)
hist(yhat.rf.test)
range(yhat.rf.test)

# Classification data
train_class <- train_df_2
test_class <- test_df_2

train_class$top40 <- as.numeric(train_class$Peak.Position <= 40)
test_class$top40 <- as.numeric(test_class$Peak.Position <= 40)

train_class$Peak.Position <- NULL
test_class$Peak.Position <- NULL
sum(train_class$top40) / nrow(train_class)

# Create oversampled training dataset to account for imbalance between top40 and not top40

top40_index <- which(train_class$top40 == 1)
set.seed(20)
oversampled_index <- sample(top40_index, sum(train_class$top40 == 0) - sum(train_class$top40 == 1), replace=TRUE)
train_class_os <- rbind(train_class, train_class[oversampled_index,])



# Perform logistic regression

library(bestglm)

fit.glm.full <- glm(top40 ~ ., train_class_os, family='binomial')
summary(fit.glm.full)


pred.full.train <- as.numeric(predict(fit.glm.full, train_class_os, type='response') >= 0.5)

mce.full.train <- mean(pred.full.train != train_class_os$top40)

pred.full.test <- as.numeric(predict(fit.glm.all, test_class, type='response') >= 0.5)

mce.full.test <- mean(pred.full.test != test_class$top40)

cm.full <- table(pred.full.test, test_class$top40)

cm.full

cm.full[2,2]/sum(test_class$top40 == "1")

# Prune down the variables to feed into bestglm

fit.glm.prune <- glm(top40 ~  acousticness + danceability + energy + liveness +
                       loudness + rap + rock + artist.pop + trap + indie + metal + country + folk +
                       funk, train_class_os, family = 'binomial')

summary(fit.glm.prune)

Xy <- model.matrix(top40 ~  acousticness + danceability + energy + liveness +
                     loudness + rap + rock + artist.pop + trap + indie + metal + country + folk +
                     funk + 0 , train_class_os) 


Xy <- data.frame(Xy, train_class_os$top40)

fit.all <- bestglm(Xy, family = binomial, method = "exhaustive", IC="AIC", nvmax = 15)

fit.all$BestModel

fit.glm <- glm(top40 ~ danceability + liveness + rap + liveness + artist.pop + trap + indie +metal + country +
               folk + funk, train_class_os,  family = 'binomial')

summary(fit.glm)

pred.glm.train <- as.numeric(predict(fit.glm, train_class_os, type='response') >= 0.5)

mce.glm.train <- mean(pred.glm.train != train_class_os$top40)

pred.glm.test <- as.numeric(predict(fit.glm, test_class, type='response') >= 0.5)

mce.glm.test <- mean(pred.glm.test != test_class$top40)

cm.glm <- table(pred.glm.test, test_class$top40)
cm.glm

cm.glm[2,2]/sum(test_class$top40 == "1")

# Classification tree

tree.class <- rpart(top40 ~ ., train_class_os)
pred.tree.train <- as.numeric(predict(tree.class, train_class_os) >= 0.5)
mce.tree.train.class <- mean(pred.tree.train != train_class_os$top40)

pred.tree.test <- as.numeric(predict(tree.class, test_class) >= 0.5)
mce.tree.test.class <- mean(pred.tree.test != test_class$top40)
cm.tree <-table(pred.tree.test, test_class$top40)
cm.tree[2,2]/sum(test_class$top40 == "1")


# Classification RF with default mtry

par(mfrow=c(1,1))

fit.rf.class <- randomForest(as.factor(top40) ~ ., train_class_os, ntree=500) 
plot(fit.rf.class)

fit.rf.pred.train <- predict(fit.rf.class, train_class_os, type="response") 
mce.rf.train <- mean(train_class_os$top40 != fit.rf.pred.train)

fit.rf.pred.test <- predict(fit.rf.class, test_class, type="response") 
mce.rf.test <- mean(test_class$top40 != fit.rf.pred.test)

cm.rf <- table(fit.rf.pred.test, test_class$top40)
sensitivity <- cm.rf[2,2]/sum(test_class$top40 == "1")

test_df_2[fit.rf.pred.test == "1",]$Peak.Position

