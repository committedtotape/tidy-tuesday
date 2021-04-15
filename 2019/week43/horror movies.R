library(tidyverse)
library(lubridate)
library(extrafont)
library(here)

horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

# movie count by each month/year
# warning regarding some dates not parsed as they only contain the year of release, not full date
month_year_count <- horror_movies %>% 
  filter(!is.na(release_date)) %>% 
  mutate(month_year = floor_date(dmy(release_date), "months")) %>% 
  count(month_year) %>% 
  mutate(n = n*-1) %>% 
  filter(!is.na(month_year)) 

ggplot(month_year_count, aes(x = month_year, y = n)) +
  # rounded segment lines look more like dripping blood then squared columns
  geom_segment(aes(xend = month_year, yend = 0), colour = "red", lineend = "round", size = 4) +
  # add extra bit of drip to the october peaks
  geom_point(data = filter(month_year_count, month(month_year) == 10),
             aes(x = month_year, y = n),
             colour = "dark red", fill = "red", size = 6, shape = 21, stroke = 2) +
  geom_hline(yintercept = 0, colour = "red", size = 5) +
  # annotations
  geom_text(aes(x = as.Date("2012-12-01"), y = -110, 
              label = "What's your\nfavourite\nscary movie\nmonth?"),
              family = "YouMurderer BB",
              colour = "red",
            size = 14
            ) +
  geom_text(aes(x = as.Date("2015-10-01"), y = -130, 
                label = "October sees the highest number of horror films released,\nwhich (ironically) is not shocking at all"),
            family = "Andale Mono",
            colour = "white",
            size = 3,
            hjust = 0.5
  ) +
  geom_text(aes(x = as.Date("2013-12-01"), y = -15, 
                label = "December is not a good month for catching a horror movie"),
            family = "Andale Mono",
            colour = "white",
            size = 3,
            hjust = 0
  ) +
  # some xtra drip on the drip to link october peaks to annotation
  geom_segment(data = filter(month_year_count, month(month_year) == 10, year(month_year) %in% c(2014, 2015, 2016)),
    aes(x = month_year, xend = month_year,
                   y = -125, yend = n-5),
               colour = "red", size = 1.5, lineend = "round", linetype = 3) +
  # axis labels
  scale_x_date(date_breaks  = "years", date_labels = "%Y", position = "top") +
  scale_y_continuous(breaks = seq(0,-150,-25), labels = seq(0,150,25), position = "right") +
  labs(caption = "Graphic: @committedtotape\nSource: IMDb",
       x = "Number of Horror Movie Releases by month", y = "") +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray20", colour = "gray20"),
        axis.title = element_text(colour = "white", family = "YouMurderer BB",
                                    size = 14),
        axis.text.x.top = element_text(colour = "white", angle = 45, family = "YouMurderer BB",
                                       hjust = 1, size = 14),
        axis.text.y.right = element_text(colour = "white", family = "YouMurderer BB",
                                         size = 14),
        plot.caption = element_text(colour = "red", family = "Courier New", size = 10),
        plot.margin = margin(10,10,10,10))
  
ggsave(here("2019","week19","horror movie releases.png"), width = 8, height = 8)
