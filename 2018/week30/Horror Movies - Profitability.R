# tidy tuesday 30 - horror movies and profit

library(tidyverse)
library(scales)
library(ggrepel)
library(RColorBrewer)
library(extrafont)

# loading scary font - YouMurderer BB!
font_import()
loadfonts()
fonttable()
fonts()

# read-in data
movies <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/2018-10-23/movie_profit.csv")

# Tidy data - only look at horror movies - group movies based on gross takings over budget
horror_movies <- movies %>% 
  select(-X1) %>% 
  filter(genre == "Horror", worldwide_gross > 0) %>% 
  mutate(gross_budget_ratio = worldwide_gross / production_budget,
         profit = case_when(gross_budget_ratio < 1 ~ "Loss",
                            gross_budget_ratio < 5 ~ "1 - 5 times Budget",
                            gross_budget_ratio < 10 ~ "5 - 10 times Budget",
                            gross_budget_ratio < 50 ~ "10 - 50 times Budget",
                            TRUE ~ "50+ times Budget"),
         profit = fct_reorder(profit, gross_budget_ratio, .desc = TRUE))

# red colour palette
reds <- rev(brewer.pal(5, "Reds"))

# plot data
ggplot(horror_movies, aes(x = production_budget, y = worldwide_gross)) +
  geom_point(aes(colour = profit), size = 2, alpha = 0.8) +
  geom_label_repel(data = filter(horror_movies, profit == "50+ times Budget"),
            aes(label = movie, colour = profit), fill = "gray10", 
            family = "AmericanTypewriter-Condensed", show.legend = FALSE) +
  geom_abline(slope = 1, intercept = 0, colour = "gray50", size = 1.5, linetype = "solid") +
  annotate("text", x = 100000000, y = 15000000, 
           label = "Break Even Line", colour = "gray80", hjust = 1.01,
           family = "American Typewriter") +
  geom_curve(aes(x = 100000000, xend = 100000000, y = 15000000, yend = 90000000),
             arrow = arrow(type = "open", length = unit(0.2,"cm")),
             colour = "gray80", size = 0.3) +
  scale_colour_manual(values = reds) +
  scale_x_log10(breaks = c(10^5, 10^6, 10^7, 10^8), labels = scales::dollar) +
  scale_y_log10(breaks = c(10^3, 10^4, 10^5, 10^6, 10^7, 10^8), labels = scales::dollar) +
  labs(x = "Production Budget (Log Scale)", y = "Worldwide Gross (Log Scale)",
       colour = NULL,
       title = "Here's...Money!!! - The Profitability of Horror Movies",
       subtitle = "Movies with shockingly high profitability (making more than 50 times their Production Budget) are shown",
       caption = "@committedtotape | Source: The Numbers, OpusData, 538") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "gray10"),
        panel.grid = element_line(colour = "gray70"),
        text = element_text(colour = "gray60", family = "AmericanTypewriter-Condensed"),
        legend.position = c(0.81, 0.4),
        legend.background = element_rect(fill = "gray10", colour = "darkred"),
        legend.text = element_text(size = 10),
        axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 2, l = 0, unit = "pt")),
        plot.title = element_text(size = 30, colour = "darkred", face = "bold", 
                                  family = "YouMurderer BB"),
        plot.caption = element_text(size = 10, colour = "darkred"),
        plot.margin = margin(t = 10, r = 20, b = 10, l = 10, unit = "pt"))

ggsave("Horror Movie Profitability.png", width = 12, height = 7)
