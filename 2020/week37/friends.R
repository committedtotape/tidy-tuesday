library(tidyverse)
library(ggbump)
library(extrafont)
library(here)

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

# get the names of main 6 characters

main_chars <- friends %>% 
  count(speaker, sort = TRUE) %>% 
  slice_head(n = 6) %>% 
  pull(speaker)

# filter for 6 main characters
# count up utterances per season and rank
lines_by_season <- friends %>% 
  filter(speaker %in% main_chars) %>% 
  count(season, speaker, name = "lines") %>% 
  group_by(season) %>% 
  arrange(season, -lines) %>% 
  mutate(rank= row_number()) %>% 
  separate(speaker, into = c("firstname", "lastname"), remove = FALSE)

# colour palette
pal <- c("#E93F33", "#FCDC33", "#42A2D6", "#9C61FD", "#AC6958", "#122B9E")

# plot
ggplot(lines_by_season, aes(season, rank, color = firstname)) +
  geom_point(size = 7) +
  geom_text(data = lines_by_season %>% filter(season == 1),
            aes(x = season - .2, label = firstname), size = 6, hjust = 1,
            family = "Gabriel Weiss' Friends Font") +
  geom_text(data = lines_by_season %>% filter(season == 10),
            aes(x = season + .2, label = firstname), size = 6, hjust = 0,
            family = "Gabriel Weiss' Friends Font") +
  geom_bump(size = 2, smooth = 5) +
  scale_x_continuous(limits = c(0, 11),
                     breaks = seq(1, 10, 1),
                     position = "top") +
  scale_y_reverse(breaks = seq(1,6,1), position = "left") +
  scale_color_manual(values = pal) +
  labs(title = "Aww, Pheebs!",
       subtitle = "Friends ranked by number of lines per season\nPhoebe has the fewest lines in 5 out of the 10 seasons, with a highest rank of 4th",
       caption = "Data: friends R package by Emil Hvitfeldt | Graphic: @committedtotape",
       x = "Season",
       y = "Rank") +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "gray10", colour = "gray10"),
        axis.text.x = element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                   size = 16),
        axis.text.y = element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                   size = 16),
        axis.title.x.top = element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                   size = 16, hjust = 0.5,
                                   margin = margin(0, 0, 10, 0)),
        axis.title.y.left = element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                        size = 16,  hjust = 0, vjust = 0.5, angle = 0),
        plot.title = element_text(family = "Gabriel Weiss' Friends Font", color = "#9C61FD",
                                  hjust = 0.5, size = 34, margin = margin(0, 0, 10, 0)),
        plot.subtitle = element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                     hjust = 0.5, size = 16, margin = margin(0, 0, 20, 0)),
        plot.caption= element_text(family = "Gabriel Weiss' Friends Font", color = "gray90",
                                     hjust = 1, size = 10, margin = margin(20, 0, 5, 0)),
        plot.margin = margin(20, 20, 20, 20))

ggsave(here("2020","week37","friends_lines_ranked.png"), width = 15, height = 7)  
