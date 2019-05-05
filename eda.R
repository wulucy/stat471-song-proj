library(ggplot2)

data <- read.csv('df2000_grouped_morefactors.csv')

# FREQUENCY PLOTS

# 10 most popular artists (2000-2018)
data %>%
  group_by(artist) %>%
  summarize(Count=n()) %>%
  arrange(desc(Count)) %>%
  head(10)

# Song release dates
ggplot(data, aes(x=Month)) + geom_histogram()

# Song Season
data.byseason <- data %>%
  group_by(Season) %>%
  summarize(Count=n())
bp <- ggplot(data.byseason, aes(x="", y=Count, fill=Season)) + geom_bar(width = 1, stat = "identity")
bp + coord_polar("y", start=0) + 
  geom_text(aes(y = Count/4 + c(0, cumsum(Count)[-length(Count)]), 
                label = (Count/100)), size=5) # need to fix labels

# OVER TIME

# Danceability, etc. over time
ggplot(data, aes(x=WeekID, y=danceability)) + geom_point() # scatter plot

data.bydanceability <- data %>%
  group_by(WeekID) %>%
  summarize(Avg.Danceability=mean(danceability))
ggplot(data.bydanceability, aes(x=WeekID, y=Avg.Danceability)) + geom_smooth()

# CORRELATIONS

data.pairs <- data %>%
  select(c(acousticness, danceability, energy, instrumentalness, liveness, loudness, speechiness))
pairs(data.pairs, cex=0.1)
                                           