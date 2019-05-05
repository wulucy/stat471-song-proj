# Linear regression - AZ
lm_df <- df2000_grouped_grouped %>%
  select(-artist, -song, -WeekID, -Weeks.on.Chart)

library(leaps)

fit.exh <- regsubsets(Peak.Position ~., lm_df, nvmax=25, method="exhaustive")
f.e <- summary(fit.exh)
names(f.e)
which.min(f.e$rss)
data.frame(variables = (1:length(f.e$rsq)),
           r_squared = f.e$rsq,
           rss = f.e$rss,
           bic = f.e$bic,
           cp = f.e$cp)

fit.backward <- regsubsets(Peak.Position ~., lm_df, nvmax=10, method="backward")

fit.final <- lm(Peak.Position ~ ., lm_df)
summary(fit.final)

fit.dance <- lm(log(Peak.Position) ~ log(danceability), lm_df)

sqrt(f.e$rss[13] / nrow(lm_df))

library(ranger)
library(rpart)

tree.full <- rpart(Peak.Position ~ ., lm_df)