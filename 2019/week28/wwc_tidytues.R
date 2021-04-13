library(tidyverse)
library(patchwork)
library(paletteer)

wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")

# remove group stages and third place playoff
wwc_knockout <- wwc_outcomes %>% 
  filter(round != "Group", round != "Third Place Playoff")

count(wwc_knockout, team, sort = TRUE)

# Create `round` variable to indicate how far in competition each team got
wwc_round <- wwc_knockout %>% 
  group_by(year, team) %>% 
  add_count() %>% 
  mutate(count = if_else(round == "Final" & win_status == "Won", n + 1, as.numeric(n))) %>% 
  summarise(round = max(count))

# create colour palette from the 80s!
colpal <- c(
            palettes_d$NineteenEightyR$sonny,
            palettes_d$NineteenEightyR$miami1,
            palettes_d$NineteenEightyR$miami2,
            palettes_d$NineteenEightyR$sunset1,
            palettes_d$NineteenEightyR$electronic_night)

# join colour palette to teams
team_cols <- bind_cols(team = unique(wwc_round$team),
                       team_col = colpal[1:23])

# add colours to dataset so they can be mapped to plot by identity
wwc_round_col <- wwc_round %>% 
  inner_join(team_cols, by = "team")

# define elements of theme
theme_wwc <- theme_minimal(base_family = "FuturaBT-Medium") +
  theme(strip.placement = "outside",
                   panel.grid = element_blank(),
                   strip.background = element_rect(fill = "#110E43", colour = "white"),
                   strip.text = element_text(colour = "white", face = "bold", size = 14),
                   text = element_text(colour = "white"),
                   axis.text = element_text(colour = "white"))

# plot 1 - tournaments pre 2015 had QF, SF and final
p1 <- wwc_round_col %>% 
  filter(year < 2015) %>% 
ggplot(aes(x = team, y = round*-1)) +
  geom_col(aes(fill = team_col), width = 1, show.legend = FALSE) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = seq(-0.5, -3.5, -1),
    labels = c("QF", "SF", "F", "W")) +
  scale_fill_identity() +
  facet_wrap(~ year, ncol = 3, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_wwc +
  theme(panel.spacing.x = unit(3, "lines"))

# plot 2 - tournaments in 2015 and 2019 also have a Round of 16
p2 <- wwc_round_col %>% 
  filter(year >= 2015) %>% 
  ggplot(aes(x = team, y = round*-1)) +
  geom_col(aes(fill = team_col), width = 1, show.legend = FALSE) +
  scale_x_discrete(position = "top") +
  scale_y_continuous(breaks = seq(-0.5, -4.5, -1),
                     labels = c("R16", "QF", "SF", "F", "W")) +
  scale_fill_identity() +
  facet_wrap(~ year, ncol = 2, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_wwc

# arrange plots, add overall title and theme using patchwork package
p1 + 
  p2 + 
  plot_layout(nrow = 2, heights = c(2, 1)) +
  plot_annotation(title = "FIFA Women's World Cup | History of Knockout Stages",
                  subtitle = "The progression of teams through the knockout stages of the Women's World Cup.\nIn 2015 the number of participants increased and a Round of 16 was introduced.",
                  caption = "Source: data.world | Graphic: @committedtotape",
  theme = theme(plot.title = element_text(hjust = 0.5, colour = "white", 
                                          face = "bold", size = 20,
                                          family = "FuturaBT-ExtraBlack"),
                plot.subtitle = element_text(hjust = 0.5, colour = "white",
                                             family = "FuturaBT-Medium"),
                plot.background = element_rect(fill = "gray20", colour = "gray20"),
                plot.caption = element_text(colour = "white", size = 11,
                                            family = "FuturaBT-BoldCondensed")))

ggsave("womens_world_cup.png", width = 11, height = 8)
