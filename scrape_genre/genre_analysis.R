data <- read.csv('scrape_genre/genre_factor_df.csv')

# GENRE AS A FACTOR

genre.data <- data %>%
  select(-c(genre))

# Get list of unique genres
df2000.genre <- merge(df2000_grouped.seasons, genre.data, by='artist')

df2000.genre.factors <- df2000.genre %>%
  select(-c(Weeks.on.Chart, artist, song, X))

fit <- lm(Peak.Position ~ ., data=df2000.genre.factors)
summary(fit) # R-squared ~0.13

