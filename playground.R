library(RSQLite)

filename <- "billboard-200.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## Some operations
tables <- dbListTables(db)
acoustic.tb <- dbReadTable(db, "acoustic_features")
albums.tb <- dbReadTable(db, "albums")

# # Sort by time and date
# merged.tb <- merge(acoustic.tb, albums.tb, by=c("artist", "album"))
# 
# # Sort by rank
# sorted <- merged.tb[order(merged.tb$rank, merged.tb$date.y), ]
# clean.sorted <- sorted[, c(-3, -18, -19, -20, -23, -24)]
# 
# colnames(clean.sorted)[17] <- "date"
# names(clean.sorted)
# 
# # Subset of 2017
# set.seed(10)
# sub2017 <- subset(clean.sorted, format(as.Date(date),"%Y")==2017)
# sub2017.clean <- sub2017[, c(-1, -2, -3, -17)]
# dim(sub2017.clean)
# 
# train.index <- sample(nrow(sub2017.clean), floor(0.7*nrow(sub2017.clean)))
# sub2017.clean.train <- sub2017.clean[train.index,]
# sub2017.clean.test <- sub2017.clean[-train.index,]
# 
# valid.index <- sample(nrow(sub2017.clean.test), floor(0.1*nrow(sub2017.clean.test)))
# sub2017.clean.validation <- sub2017.clean.test[valid.index,]
# sub2017.clean.test <- sub2017.clean.test[-valid.index,]
# 
# # LASSO
# sub2017.clean.train$rank <- as.numeric(sub2017.clean.train$rank)
# Y <- sub2017.clean.train$rank
# X <- as.matrix(sub2017.clean.train[, -c(14)])
# 
# result.lasso <- cv.glmnet(X, Y, alpha=1, nfolds=10)
# plot(result.lasso)
# lasso.lambda.1se <- result.lasso$lambda.1se
# lasso.lambda.1se
# 
# beta.lasso <- coef(result.lasso, s="lambda.1se") 
# beta <- beta.lasso[which(beta.lasso !=0),] 
# beta <- as.matrix(beta)
# beta <- rownames(beta)
# 
# glm.input <- as.formula(paste("rank", "~", paste(beta[-1],collapse = "+")))
# result.glm <- glm(glm.input, data=sub2017.clean.train) 
# result.glm.coef <- coef(result.glm)
# result.glm.coef
# summary(result.glm)


# Reading data
bill100 <- read.csv(file = "Hot Stuff.csv")
colnames(bill100)[4] <- "song"
colnames(bill100)[5] <- "artist"


# Merging with World Data
merged.tb <- merge(bill100, acoustic.tb, by=c("artist", "song"))
merged.tb <- merged.tb[, -c(3, 5, 6, 7, 8, 11, 26, 27)]
merged.tb$WeekID <- as.Date(merged.tb$WeekID, "%m/%d/%Y")
names(merged.tb)

# Grouping
merged.tb2 <- merged.tb %>%
  group_by(
    song, 
    artist, 
    album, 
    acousticness, 
    danceability, 
    duration_ms, 
    energy, 
    instrumentalness, 
    key, 
    liveness, 
    loudness, 
    mode, 
    speechiness, 
    tempo, 
    time_signature, 
    valence) %>%
  summarise(
    WeekID = min(WeekID),
    Peak.Position = max(Peak.Position),
    Weeks.on.Chart = max(Weeks.on.Chart)
  )

# Subset of songs after 2000
df2000 <- as.data.frame(subset(merged.tb2, format(WeekID,"%Y")>=2000))
df2000 <- df2000[order(df2000$WeekID), ]
dim(df2000)

df2000_grouped <- df2000 %>%
  group_by(song, artist, WeekID, Peak.Position, Weeks.on.Chart) %>%
  summarise(
    acousticness = round(mean(acousticness), 3),
    danceability = round(mean(danceability), 3),
    duration_ms = as.integer(mean(duration_ms)),
    energy = round(mean(energy), 3),
    instrumentalness = mean(instrumentalness),
    key = as.integer(mean(key)),
    liveness = round(mean(liveness), 3),
    loudness = round(mean(loudness), 3),
    mode = as.integer(mean(mode)),
    speechiness = round(mean(speechiness), 4),
    tempo = round(mean(tempo), 3),
    time_signature = as.integer(mean(time_signature)),
    valence = round(mean(valence), 3)
    )

df2000_grouped = as.data.frame(df2000_grouped)

# Add factors
# Create season factor
# Winter = 12, 1, 2; Spring = 3, 4, 5, Summer = 6,7,8 Fall = 9, 10, 11
df2000.seasons <- df2000 %>%
  mutate(Month=as.numeric(format(WeekID, "%m"))) %>%
  mutate(Season=
           case_when(
             Month == 12 | Month <= 2 ~ "Winter",
             Month >= 3 & Month <= 5 ~ "Spring",
             Month >= 6 & Month <= 8 ~ "Summer",
             Month >= 9 & Month <= 11 ~ "Fall",
             TRUE ~ "NA"
           )
  )

# Create artist popularity factor
# artist.pop = number of hot 100 songs by artist in past 3 years
library(lubridate)

df1997 <- as.data.frame(subset(merged.tb2, format(WeekID,"%Y")>=1997))
df1997 <- df1997[order(df1997$WeekID), ] # get songs since 1997

artist.pop.list <- c()
for (idx in 1:nrow(df2000)) {
  WeekID <- df2000[idx, "WeekID"]
  artist <- df2000[idx, "artist"]
  three.y.earlier <- WeekID %m-% months(12*3)
  artist.pop <- nrow(df1997[(df1997$artist == artist) & (df1997$WeekID < WeekID) & (df1997$WeekID >= three.y.earlier),])
  artist.pop.list <- c(artist.pop.list, artist.pop)
}

df2000.seasons$artist.pop <- artist.pop.list

# Linear regression - AZ
lm_df <- df2000_grouped %>%
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
