# Tidy Tuesday Week 2 - IMDb ratings
library(tidyverse)
library(gghighlight)
library(extrafont)

loadfonts()
fonttable()

# read in data
ratings <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv")

# Select shows with no missing seasons, and at least 5 seasons long
show_summary_selected <- ratings %>% 
  group_by(title) %>% 
  summarise(max_season = max(seasonNumber),
            seasons = n()) %>% 
  filter(max_season == seasons, seasons >= 5) %>% 
  ungroup()

# Join back to keep full data for these selected titles
ratings_selected <- ratings %>% 
  semi_join(show_summary_selected, by = "title")

# Get season with the highest avg. rating for each title
ratings_selected_max <- ratings_selected %>% 
  group_by(title) %>% 
  mutate(max_rating = max(av_rating)) %>% 
  filter(max_rating == av_rating) %>% 
  select(title, max_rating, best_season = seasonNumber)

# Get season with the lowest avg. rating for each title
ratings_selected_min <- ratings_selected %>% 
  group_by(title) %>% 
  mutate(min_rating = min(av_rating)) %>% 
  filter(min_rating == av_rating) %>% 
  select(title, min_rating, worst_season = seasonNumber)

# Combine highest and lowest seasons
ratings_selected_best_worst <- ratings_selected_max %>% 
  inner_join(ratings_selected_min, by = "title") 

# Get top 10 titles in terms of biggest % drop from high point to subsequent low point
ratings_selected_fall <- ratings_selected_best_worst %>% 
  filter(worst_season > best_season) %>% 
  mutate(percent_drop = (max_rating - min_rating) / max_rating) %>% 
  arrange(desc(percent_drop)) %>% 
  select(title, percent_drop, max_rating, min_rating) %>% 
  head(10)

# join the % drop figure for these 10 titles to highlight them in plot
# also create a label for the relevant record for the highest and lowest ratings 
ratings_selected_final <- ratings_selected %>% 
  left_join(ratings_selected_fall, by = "title") %>% 
  ungroup() %>% 
  mutate(max_label_rating = case_when(av_rating == max_rating ~ max_rating,
                                  TRUE ~ NaN),
         min_label_rating = case_when(av_rating == min_rating ~ min_rating,
                                      TRUE ~ NaN))

# plot to highlight the 10 titles with the biggest % drop
# make unhighlighted seasons a light, transparent grey
# facet by the 10 titles so plot isn't cluttered

p <- ggplot(ratings_selected_final, aes(x = seasonNumber, y = av_rating)) +
  geom_line(aes(colour = title), show.legend = FALSE, size = 1) +
  gghighlight(!is.na(percent_drop), use_direct_label = FALSE, use_group_by = FALSE,
              unhighlighted_colour = alpha("gray70", 0.4)) +
  scale_colour_manual(values = rep("#35274A", 10)) +
  scale_y_continuous(limits = c(5, 10), expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 20), breaks = c(5, 10, 15, 20)) +
  facet_wrap(~ title, nrow = 2) +
  labs(x = "Season", y = "IMDb Average Season Rating",
       title = "Did these 10 shows 'Jump The Shark'?",
       subtitle = "Shows with biggest % drop in rating between peak and subsequent low point.\nTV Dramas shown in America since 1990 (minimum 5 Seasons)",
       caption = "@committedtotape | Data: IMDb") +
  theme_minimal() +
  theme(text = element_text(colour = "white", family = "FuturaBT-Medium"),
        strip.text = element_text(colour = "#35274A", face = "bold", size = 10),
        axis.text = element_text(colour = "white"),
        strip.background = element_rect(fill = "gray70", colour = "#35274A"),
        plot.background = element_rect(fill = "gray40"),
        panel.grid.major = element_line(size = 0.5, colour = "gray50"),
        panel.grid.minor = element_line(size = 0.2, colour = "gray50"),
        plot.title = element_text(colour = "#35274A", size = 18, family = "FuturaBT-Heavy"))

# add the labels for highs and lows as created in final data step
p +  
  geom_text(aes(label = round(max_label_rating, 1)), colour = "#35274A", vjust = -0.7, family = "FuturaBT-Heavy") +
  geom_text(aes(label = round(min_label_rating, 1)), colour = "#35274A", vjust = 1.5, family = "FuturaBT-Heavy")

ggsave("TV Dramas Top 10 Bombs.png", width = 15, height = 7)
