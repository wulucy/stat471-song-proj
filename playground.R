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
unique(bill100$Song)

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
