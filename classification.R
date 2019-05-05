df2000.seasons <- read.csv('df2000_grouped_morefactors.csv')
df2000.seasons$top40 <- ifelse(df2000.seasons$Peak.Position <= 40, 1, 0)
df2000.seasons$Season <- as.factor(df2000.seasons$Season)
names(df2000.seasons)

Y <- df2000.seasons$top40
X <- as.matrix(df2000.seasons[, c(6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 21)]) 
X

result.lasso <- cv.glmnet(X, Y, alpha=1, family="binomial")
plot(result.lasso)
lasso.lambda.1se <- result.lasso$lambda.1se
lasso.lambda.1se
result.lasso$cvm

predict.lasso <- predict(result.lasso, as.matrix(data2.test[, -1]), type = "class", s="lambda.1se")
mean(data2.test$rating != predict.lasso)