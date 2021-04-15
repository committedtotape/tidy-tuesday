library(tidyverse)
library(ggtext)
library(extrafont)
library(here)

pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")
pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")

answer_levels <- c("Never Again", "Poor", "Average", "Good", "Excellent")

# proportion of answers per place from jareds data
pizza_jared_totals <- pizza_jared %>% 
  mutate(#answer = if_else(answer == "Fair", "Average", answer),
         answer = fct_relevel(answer, answer_levels)) %>% 
  group_by(place, answer) %>% 
  summarise(votes = sum(votes),
            total_votes = sum(total_votes)) %>% 
  group_by(place) %>% 
  ungroup() %>% 
  mutate(percent = votes/total_votes)

count(pizza_jared_totals, answer)

# barstool overall average for NY places
pizza_barstool <- pizza_barstool %>% 
  filter(city == "New York") %>% 
  select(place = name, barstool_avg_score = review_stats_all_average_score)

# join pizzas from jared's data to pizza from barstool
# some ambiguous names from barstool, only keep those with a single match i.e. 5 records
# Roccos has inconsistent likert scale so removed
# also, only keep places with min 10 votes in jareds data
pizza_both <- pizza_jared_totals %>% 
  inner_join(pizza_barstool, by = "place") %>% 
  add_count(place) %>% 
  filter(n == 5,
         total_votes >= 10,
         place != "Rocco's Pizza Joint") %>% 
  mutate(barstool_avg_score1 = barstool_avg_score/10,
         answer_num = as.integer(answer))

count(pizza_both, place)    

# let's make some pizza
ggplot(pizza_both, aes(x = answer_num)) +
  # start with the base
  geom_area(aes(group = 1, y = 1), colour = "#CD9761", fill = "#CD9761") +
  # spread over the tomato sauce
  geom_area(aes(group = 1, y = barstool_avg_score1), colour = "#C32B0E", fill = "#C32B0E") +
  # sprinkle the cheese
  geom_area(aes(group = 1, y = percent), colour = "#F4B043", fill = "#F4B043") +
  # cut slices
  geom_segment(aes(x = 2, xend = 2, y = 0, yend = 1), colour = "#8CC6E5") +
  geom_segment(aes(x = 3, xend = 3, y = 0, yend = 1), colour = "#8CC6E5") +
  geom_segment(aes(x = 4, xend = 4, y = 0, yend = 1), colour = "#8CC6E5") +
  # remove a slice
  scale_x_continuous(expand = c(0.1,0.1), breaks = 1:5, labels = answer_levels) +
  scale_y_continuous(limits = c(0,1)) +
  facet_wrap(~ place) +
  coord_polar() +
  labs(title = "Where to go for Pizza in New York",
       subtitle = "12 Pizzerias in New York, rated by both NY Open Stats meet up (min. 10 votes) and Barstool Sports users<br>
    The <span style='color:#C32B0E'>tomato sauce</span> coverage represents the <span style='color:#C32B0E'>Barstool rating</span> - if it goes right to the edge it's 10/10!<br>
    The <span style='color:#F4B043'>cheese</span> coverage represents the <span style='color:#F4B043'>NY Open Stats ratings</span> as proportion of votes on likert scale ('Never Again' to 'Excellent')<br>
    going clockwise from top, so <span style='color:#F4B043'>A cheesy glob in the North West section is what we're after!</span>",
       caption = "Source: Jared Lander / Tyler Richards\nGraphic: committedtotape") +
  theme_void(base_family = "Phosphate Solid") +
  theme(plot.background = element_rect(fill = "#8CC6E5", colour = "#8CC6E5"),
        plot.title = element_text(colour = "#54743D", family = "Phosphate Inline", size = 20, hjust = 0.5),
        plot.subtitle = element_markdown(size = 11, margin = margin(10,10,10,10)),
        strip.text = element_text(size = 12, colour = "#54743D"))

ggsave(here("2019","week40","pizza_ratings.png"), width = 10, height = 9)

# eat pizza, go to sleep

    
    
    

