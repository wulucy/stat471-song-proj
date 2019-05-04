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
sub2017 <- subset(clean.sorted, format(as.Date(date),"%Y")==2017)
