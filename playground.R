install.packages("RSQLite")
library(RSQLite)

filename <- "billboard-200.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)

## Some operations
tables <- dbListTables(db)
acoustic.tb <- dbReadTable(db,"acoustic_features")
albums.tb <- dbReadTable(db,"albums")

# commiting