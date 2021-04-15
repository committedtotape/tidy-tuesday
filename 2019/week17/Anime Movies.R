library(tidyverse)
library(lubridate)
library(paletteer) #all the palettes
library(here)

tidy_anime <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-23/tidy_anime.csv")

# remove duplicate entries because of multiple genres
# only keep those scored by at least 100 users
tidy_anime_unique <- tidy_anime %>% 
  distinct(animeID, .keep_all = TRUE) %>% 
  filter(scored_by >= 100, !is.na(score), !is.na(start_date)) %>% 
  mutate(decade = (year(start_date) %/% 10) * 10)

# focus just on films since 1960
tidy_anime_movie <- tidy_anime_unique %>% 
  filter(type == "Movie", start_date >= as.Date("1960-01-01")) %>% 
  mutate(decade = paste0(decade, "s"))

# top 5 most rated films to be highlighted in plot
movie_most_scored <- tidy_anime_movie %>% 
  top_n(5, scored_by) 

# background colour
back_col <- paletteer_d(ghibli, MarnieLight1)[2]

# plot
ggplot(tidy_anime_movie, aes(x = decade, y = score)) +
  geom_jitter(aes(size = scored_by, fill = decade), 
              width = 0.1, alpha = 0.4, shape = 21,
              colour = ifelse(tidy_anime_movie$animeID %in% movie_most_scored$animeID, "white", "black")) +
  geom_boxplot(aes(fill = decade), colour = "white", width = 0.4,
               show.legend = FALSE, outlier.shape = NA, alpha = 0.4) +
  geom_text(aes(x = "2000s", y = 9.5, 
                label = "Movies with the most user ratings\ntend to have the highest scores"),
            size = 2.5, colour = "white", hjust = 0.5,
            family = "AnimeAce") +
  scale_fill_paletteer_d(ghibli, PonyoMedium, direction = -1) +
  scale_color_paletteer_d(ghibli, PonyoMedium, direction = -1) +
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
  scale_size_continuous(breaks = seq(100000, 500000, 100000), 
                        labels = paste0(seq(100, 500, 100), "k")) +
  labs(x = "", y = "Score", 
       size = "Bubble size represents no. of users that scored movie",
       title = "Are Anime Movies getting better?",
       subtitle = "Anime movies since the 1960s as scored by MyAnimeList users",
       caption = "Movies scored by less than 100 users have been removed\n@committedtotape") +
  theme(text = element_text(colour = "white", family = "AnimeAce"),
        plot.title = element_text(family = "AnimeAceBold", size = rel(1.5)),
        plot.caption = element_text(size = rel(0.7)),
        plot.background = element_rect(fill = back_col),
        panel.background = element_rect(fill = back_col),
        panel.grid = element_line(colour = "gray40"),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(colour = "white"),
        legend.position = c(0.5, 0.15),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = back_col),
        legend.box.background = element_rect(fill = back_col),
        legend.key = element_rect(fill = back_col,  colour = back_col),
        legend.title = element_text(size = rel(0.8))) +
  guides(colour = FALSE,
         fill = FALSE,
         size = guide_legend(title.position = "top", title.hjust = 0.5,
                             override.aes = list(colour = "white", alpha = 1)))

ggsave(here("2019","week17","anime movies.png"), width = 8, height = 11)
