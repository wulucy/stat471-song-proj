df2000_grouped <- read.csv('df2000_grouped_morefactors.csv')

# Linear regression - AZ
lm_df <- df2000_grouped %>%
  select(-artist, -song, -WeekID, -Weeks.on.Chart, -X)

set.seed(22)
train_indices <- sample(nrow(lm_df), nrow(lm_df) * 0.7, replace=TRUE)
train_df <- lm_df[train_indices,]
test_df <- lm_df[-train_indices,]

# Consider transformations
fit.linear <- lm(Peak.Position ~., lm_df)
fit.log <- lm(log(Peak.Position) ~. , lm_df)
fit.inverse <- lm(log(101- Peak.Position) ~., lm_df)
fit.square <- lm(Peak.Position^2 ~. , lm_df)

# Normal Q-Q Plots
par(mfrow = c(2,2))
plot(fit.linear, 2)
plot(fit.log, 2)
plot(fit.inverse, 2)
plot(fit.square, 2)

# Histograms of transformed distribution
par(mfrow = c(2,2))
hist(lm_df$Peak.Position)
hist(I(log(lm_df$Peak.Position)))
hist(I(log(101 - lm_df$Peak.Position)))
hist(I(lm_df$Peak.Position^2))


# Add transformed variable to new data frame
lm_df_1 <- lm_df
lm_df_1$Peak.Transformed <- log(101 - lm_df$Peak.Position)
lm_df_1$Peak.Position <- NULL


library(leaps)

# Exhaustive model selection with regsubsets returns model with all variables
fit.exh <- regsubsets(Peak.Transformed ~ ., lm_df_1, nvmax=25, method="exhaustive")
f.e <- summary(fit.exh)
which.min(f.e$rss)
data.frame(variables = (1:length(f.e$rsq)),
           r_squared = f.e$rsq,
           rss = f.e$rss,
           bic = f.e$bic,
           cp = f.e$cp)

# Perform manual backwards selection until all variables are significant
fit.backwards <- lm(Peak.Transformed ~., lm_df_1)
fit.backwards.1 <- lm(Peak.Transformed ~. -key, lm_df_1)
fit.backwards.2 <- lm(Peak.Transformed ~. -key -liveness, lm_df_1)
fit.backwards.3 <- lm(Peak.Transformed ~. -key -liveness -tempo, lm_df_1)
fit.backwards.4 <- lm(Peak.Transformed ~. -key -liveness -tempo -Season, lm_df_1)
fit.backwards.5 <- lm(Peak.Transformed ~. -key -liveness -tempo -Season -Month, lm_df_1)
fit.backwards.6 <- lm(Peak.Transformed ~. -key -liveness -tempo -Season -Month -time_signature, lm_df_1)
fit.backwards.7 <- lm(Peak.Transformed ~. -key -liveness -tempo -Season -Month -time_signature -loudness, lm_df_1)
fit.backwards.8 <- lm(Peak.Transformed ~ acousticness + danceability + duration_ms + duration_ms + instrumentalness
                      + mode + speechiness + valence + artist.pop, lm_df_1)
summary(fit.backwards.8)

# Our lm final model
fit.final <- fit.backwards.8

# Predicted values
yhat <- predict(fit.final, lm_df)
yhat_lm_response <- 101 - exp(yhat)
range(yhat_lm_response)
mse.lm.train <- mean((lm_df$Peak.Position-yhat_lm_response)^2)


# Regression trees
library(rpart)

tree.full <- rpart(Peak.Position ~ ., train_df)
yhat.tree.train <- predict(tree.full, train_df)
mse.tree.train <- mean((train_df$Peak.Position-yhat.tree.train)^2)
yhat.tree.test <- predict(tree.full, test_df)
mse.tree.test <- mean((test_df$Peak.Position-yhat.tree.test)^2)

# Random Forest
library(randomForest)


fit.rf <- randomForest(Peak.Position~., train_df, mtry=10, ntree=500) 

par(mfrow=c(1,1))

plot(fit.rf, col="blue", pch=16, type="p", main="default plot")

# stabilizes around ntree = 200, now choose mtry

rf.error.p <- 1:17

for (p in 1:17) {
  fit.rf <- randomForest(Peak.Position ~ ., train_df, mtry=p, ntree=200) 
  rf.error.p[p] <- fit.rf$mse[200] 
}

rf.error.p 

plot(1:17, rf.error.p, pch=16, xlab="mtry",
     ylab="OOB mse of mtry") 
lines(1:17, rf.error.p)

# choose mtry = 6 for final rf
fit.rf.final <- randomForest(Peak.Position ~ ., train_df, mtry=6, ntree=200) 

# Testing errors
plot(fit.rf.final$mse, xlab="number of trees", col="blue", ylab="ave mse up to i many trees using OOB predicted", pch=16)
fit.rf.final$mse[200]

yhat.rf.train <- predict(fit.rf.final, train_df)
mse.rf.train <- mean((train_df$Peak.Position-yhat.rf.train)^2)

yhat.rf.test <- predict(fit.rf.final, test_df)
mse.rf.test <- mean((test_df$Peak.Position-yhat.rf.test)^2)

hist(yhat.rf.train)
range(yhat.rf.train)
hist(yhat.rf.test)
range(yhat.rf.test)

# Classification data
train_class <- train_df
test_class <- test_df

train_class$top40 <- train_df$Peak.Position <= 40
test_class$top40 <- test_df$Peak.Position <= 40

train_class$Peak.Position <- NULL
test_class$Peak.Position <- NULL

# Perform logistic regression

library(bestglm)
Xy <- model.matrix(top40 ~.+0 -key - liveness -tempo -time_signature, train_class) 
Xy <- data.frame(Xy, train_class$top40)   

#fit.all <- bestglm(Xy, family = binomial, method = "exhaustive", IC="AIC", nvmax = 10)

fit.all$BestModel

fit.glm <- glm(top40 ~ danceability + duration_ms + instrumentalness + loudness + valence
               + Month + Season + artist.pop, train_class, family = 'binomial')
summary(fit.glm)


pred.glm.train <- as.numeric(predict(fit.glm, train_class, type='response') >= 0.15)

mce.glm.train <- mean(pred.glm.train != train_class$top40)

pred.glm.test <- as.numeric(predict(fit.glm, test_class, type='response') >= 0.15)

mce.glm.test <- mean(pred.glm.test != test_class$top40)

# Classification tree