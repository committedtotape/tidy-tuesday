library(tidyverse)
library(ggbeeswarm)
library(ggtext)
library(here)

volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')

count(events, event_type, sort = TRUE)

# just focus on ash and explosion events since 1920
events_filtered <- events %>% 
  filter(event_type %in% c("Explosion", "Ash"),
         eruption_start_year >= 1920)

events_filtered %>% 
  count(event_type, eruption_start_year) 

#palette for plot
pal <- c("Explosion" = "#E33D32", "Ash" = "#F2EFEF")

# plot
ggplot(events_filtered, aes(x = event_type, y = eruption_start_year)) +
  # volcano
  geom_point(aes(x = event_type, y = 1890, colour = event_type), shape = 24, size = 60, 
             fill = "#061333", stroke = 2, show.legend = FALSE) +
  # volcano top
  geom_point(aes(x = event_type, y = 1920), size = 30, colour = "#606E90") +
  # label
  geom_text(aes(x = event_type, y = 1890, colour = event_type, 
                label = str_to_upper(event_type)),
            family = "FuturaBT-Heavy",
            show.legend = FALSE) +
  # the explosions!
  geom_quasirandom(aes(colour = event_type), alpha = 0.7,
                   show.legend = FALSE) + 
  scale_colour_manual(values = pal) +
  scale_y_continuous(limits = c(1880, 2020), breaks = seq(1920,2020,10), labels = seq(1920,2020,10)) +
  labs(title = "Ash and Explosion events in the last century peaked between 2000-2010",
       subtitle = "The number of <span style='color:#F2EFEF'>**Ash**</span> and <span style='color:#E33D32'>**Explosion**</span> events each year since 1920",
       caption = "\nSource: The Smithsonian Institution\nGraphic: @committedtotape") +
  theme_void(base_family = "FuturaBT-BoldCondensed") +
  theme(plot.background = element_rect(fill = "#606E90", colour = "#606E90"),
        plot.margin = margin(10,5,20,10),
        plot.title = element_text(colour = "#061333", size = 20),
        plot.subtitle = element_markdown(colour = "#061333", size = 16),
        plot.caption = element_text(colour = "#061333", size = 14),
        axis.text.y = element_text(size = 14, colour = "#061333", family = "FuturaBT-BoldCondensed"))
  
ggsave(here("2020","week20","volcanic_eruptions.png"), width = 8, height = 9)
