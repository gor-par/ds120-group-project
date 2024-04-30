
library(dplyr)
library(ggplot2)

dataBestMovies <- read.csv("C:/Users/Asus/OneDrive/Desktop/Group project - Netflix/Best Movies Netflix.csv")
dataBestShows <- read.csv("C:/Users/Asus/OneDrive/Desktop/Group project - Netflix/Best Shows Netflix.csv")

has_missingMovie <- sum(is.na(dataBestMovies)) > 0
has_missingShow <- sum(is.na(dataBestShows)) > 0
dataBestMovies$MAIN_GENRE <- factor(dataBestMovies$MAIN_GENRE)
dataBestMovies$MAIN_PRODUCTION <- factor(dataBestMovies$MAIN_PRODUCTION)
dataBestShows$MAIN_GENRE <- factor(dataBestShows$MAIN_GENRE)
dataBestShows$MAIN_PRODUCTION <- factor(dataBestShows$MAIN_PRODUCTION)
print(summary(dataBestMovies))
print(summary(dataBestShows))

group_by_date <- function(DOF) {
  start_decade <- seq(from = 1980, to = 2020, by = 10)
  end_decade <- start_decade + 9
  for (i in length(start_decade):1) {
    if (DOF >= start_decade[i] & DOF < end_decade[i]) {
      decade <- paste0(start_decade[i], 's')
      return (decade)
    }
  }
  if (DOF < start_decade[1]) {
    return("Released before 1980")
  } else {
    return ("Released after 2020")
  }
}

dataBestMovies$DECADES <- sapply(dataBestMovies$RELEASE_YEAR, group_by_date)
dataBestShows$DECADES <- sapply(dataBestShows$RELEASE_YEAR, group_by_date)

grouppedMovies <- dataBestMovies %>%
  group_by(MAIN_GENRE, DECADES) %>%
  summarise(SCORE = mean(SCORE))
grouppedShows <- dataBestShows %>%
  group_by(MAIN_GENRE, DECADES) %>%
  summarise(SCORE = mean(SCORE))

grouppedMovies$DECADES <- factor(grouppedMovies$DECADES, 
                                 levels = c("Released before 1980", "1980s", "1990s", "2000s", "2010s", "2020s", "Released after 2020"))

ggplot(data = grouppedMovies, aes(x = '', y = SCORE, fill = MAIN_GENRE)) +
  geom_bar(width = 1, position = 'fill', stat = "identity") +
  facet_wrap(.~DECADES, nrow=3, ncol = 3) +
  labs(title = "Movie genres by their decades", 
       x = "Main Genre", 
       y = "Score") +
  scale_x_discrete() +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 5))

ggplot(data = grouppedMovies, aes(x = MAIN_GENRE, y = SCORE)) +
  geom_bar(fill = "pink", stat = "identity", col = "darkred") +
  labs(title = "Distribution Movie score by their decades", 
       x = "MAIN_GENRE", y = "SCORE") +
  coord_polar("x", start = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

groupping_genre <- grouppedMovies %>%
  group_by(MAIN_GENRE) %>%
  summarise(SCORE = mean(SCORE))

ggplot(data = groupping_genre, aes(x = reorder(MAIN_GENRE, -SCORE), y = SCORE)) +
  geom_bar(fill = "pink", stat = "identity") +
  labs(title = "Distribution Movie score by their decades", 
       x = "MAIN_GENRE", y = "SCORE") +
  coord_cartesian(ylim = (c(6.5,8))) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




