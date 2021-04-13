library(tidyverse)
library(ggTimeSeries)
library(extrafont)

loadfonts()
fonttable()

launches <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/launches.csv")
agencies <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2019/2019-01-15/agencies.csv")

count(launches, state_code, sort = TRUE)
count(launches, launch_year) %>% View()
count(launches, category)

# categorise the biggest states
launches_processed <- launches %>% 
  mutate(country = case_when(state_code %in% c("SU", "RU") ~ "Soviet Union/Russia",
                             state_code == "US" ~ "United States",
                             state_code == "CN" ~ "China",
                             state_code == "F" ~ "France",
                             TRUE ~ "Other"))

count(launches_processed, country, sort = TRUE)

# summarise launches at year and country level for plotting
launches_plot <- launches_processed %>% 
  count(launch_year, country, sort = TRUE)

# create tibble for adding labels to plot
country_labels <- launches_plot %>% 
  group_by(country) %>% 
  summarise(launch_year = max(launch_year) - 0.5) %>% 
  ungroup() %>% 
  add_column(n = c(18, -12, 0, 35, -30))

# plot steamgraph using stat_steamgraph from ggTimeSeries package 
ggplot(launches_plot, aes(x = launch_year, y = n, group = country, fill = country , colour = country)) +
  stat_steamgraph(show.legend = FALSE) +
  geom_text(data = country_labels, aes(label = country), 
            hjust = 1, colour = "black", fontface = "bold", family = "Avenir-BlackOblique") +
  scale_x_continuous(breaks = seq(1960, 2010, 10), limits = c(1957, 2018), expand = c(0,0)) +
  scale_y_continuous(breaks = seq(-60, 60, 20), labels = abs(seq(-60, 60, 20)), expand=c(0,0)) +
  scale_fill_viridis_d(option = "C", alpha = 0.7) +
  scale_colour_viridis_d(option = "C") +
  labs(x = "Year of Launch", y = "Number of Launches",
       title = "Space Launches by Country",
       subtitle = "China are an emerging force in the Space Race\n",
       caption = "@committedtotape | Source: The Economist") +
  theme_minimal() +
  theme(text = element_text(colour = "gray30", family = "Avenir-BlackOblique"),
        plot.title = element_text(face = "bold", size = 16),
        plot.background = element_rect(fill = "gray10"),
        panel.grid.major = element_line(size = 0.5, colour = "gray30"),
        panel.grid.minor = element_line(size = 0.1, colour = "gray30"))

ggsave("Space Race.png", width = 13, height = 7)
