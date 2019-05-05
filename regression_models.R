df2000_grouped <- read.csv('df2000_grouped_morefactors.csv')

# Linear regression - AZ
lm_df <- df2000_grouped %>%
  select(-artist, -song, -WeekID, -Weeks.on.Chart)

library(leaps)
fit.exh <- regsubsets(Peak.Position ~ . -X, lm_df, nvmax=25, method="exhaustive")
f.e <- summary(fit.exh)
which.min(f.e$rss)
data.frame(variables = (1:length(f.e$rsq)),
           r_squared = f.e$rsq,
           rss = f.e$rss,
           bic = f.e$bic,
           cp = f.e$cp)

fit.backward <- regsubsets(Peak.Position ~., lm_df, nvmax=10, method="backward")

fit.final <- lm(Peak.Position ~  -X., lm_df)
summary(fit.final)

fit.dance <- lm(Peak.Position ~ danceability, lm_df)

yhat_lm <- predict(fit.final, lm_df)
mse.lm.train <- mean((lm_df$Peak.Position - yhat_lm)^2)

yhat_lm_dance <- predict(fit.dance, lm_df)
mse.dance.train <- mean((lm_df$Peak.Position - yhat_lm_dance)^2)

library(ranger)
library(rpart)

tree.full <- rpart(Peak.Position ~ . -X, lm_df)

yhat_tree <- predict(tree.full, lm_df)
mse.tree.train <- mean((lm_df$Peak.Position-yhat_tree)^2)


top_10 <- df2000_grouped %>%
  filter(as.integer(Peak.Position) <= 10)


