# Tidy Tuesday Week 21 - Californian Wildfires
library(tidyverse)
library(ggridges)

# read in data from github
calfire <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week21/week21_calfire_frap.csv")

# inspect data
glimpse(calfire)

count(calfire, year_)

summary(calfire$gis_acres)

count(calfire, cause)

# tidy up data, create decade and cause of fire
calfire1 <- calfire %>%  select(year_, cause, gis_acres) %>% 
  filter(!is.na(gis_acres)) %>% 
  mutate(decade = 10 * floor(year_ / 10),
         cause = case_when(cause %in% c(1, 17) ~ "Natural",
                           cause == 14 | is.na(cause) ~ "Unknown",
                           TRUE ~ "Human"))

count(calfire1, cause)

average <- calfire1 %>% 
  group_by(decade, cause) %>% 
  summarise(mean = mean(gis_acres)) 

# plot density ridges for each decade, faceted by cause
# https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html

ggplot(calfire1, aes(x = gis_acres, y = decade, group = decade, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, alpha = 0.3, color = "white",
                               size = 0.3, show.legend = FALSE) +
  scale_x_log10(breaks = c(0.1, 1, 10, 10^2, 10^3, 10^4, 10^5), 
                labels = c("0.1", "1", "10", "100", "1k", "10k", "100k")) +
  scale_y_reverse(breaks = seq(1950, 2010, 10), labels = paste0(seq(1950, 2010, 10), "s")) +
  scale_fill_viridis_c(option = "B") +
  facet_wrap(~ cause, nrow = 3) +
  coord_cartesian(xlim = c(0.01, 600000)) +
  labs(x = "Acres Burned (GIS calculated) on Log Scale", y = "Decade", title = "Californian Wildfires",
       subtitle = "Distribution of Acres Burned by Decade and Cause",
       caption = "SOURCE:Cal Fire/BuzzFeed") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "gray20"),
        panel.grid.major = element_line(colour = "white", size = 0.3),
        axis.text = element_text(colour = "white"),
        strip.text = element_text(colour = "white", size = 12),
        text = element_text(colour = "white"),
        plot.title = element_text(colour = "gold", size = 18, face = "bold"),
        plot.subtitle = element_text(size = 14))

ggsave("Cal Fires Acres Burned Density Ridges.png", width = 11, height = 7)
