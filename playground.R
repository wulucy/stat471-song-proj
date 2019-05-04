library(RSQLite)

filename <- "billboard-200.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## Some operations
tables <- dbListTables(db)
acoustic.tb <- dbReadTable(db,"acoustic_features")
albums.tb <- dbReadTable(db,"albums")

# Sort by time and date
merged.tb <- merge(acoustic.tb, albums.tb, by=c("artist", "album"))

# Sort by rank
sorted <- merged.tb[order(merged.tb$rank, merged.tb$date.y), ]
clean.sorted <- sorted[, c(-3, -18, -19, -20, -23, -24)]

colnames(clean.sorted)[17] <- "date"
names(clean.sorted)

# Subset of 2017
set.seed(10)
sub2017 <- subset(clean.sorted, format(as.Date(date),"%Y")==2017)
dim(sub2017)

train.index <- sample(nrow(sub2017), floor(0.7*nrow(sub2017)))
sub2017.train <- sub2017[train.index,]
sub2017.test <- sub2017[-train.index,]

valid.index <- sample(nrow(sub2017.test), floor(0.1*nrow(sub2017.test)))
sub2017.validation <- sub2017.test[valid.index,]
sub2017.test <- sub2017.test[-valid.index,]


