library(tidyverse)
library(glue)
library(ggtext)
library(here)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv')
#artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv")

mondrian <- artwork %>% 
  filter(artist == "Mondrian, Piet") %>% 
  select(title, year, acquisitionYear, width, height) %>% 
  arrange(year) %>% 
  mutate(width_cm = width / 100,
         height_cm = height / 100,
         # not actually cm - just scaling so it looks best in graph
         xmin = year,
         xmax = xmin + width_cm,
         ymin = acquisitionYear,
         ymax = ymin + height_cm,
         block_col = c("#B54B26","#EDD358", "#262829", "#4C73AC", "#999496"),
         text = glue("<span style='color:{block_col}'>{title} | {year} | {width} x {height}</span>")
  )


ggplot(mondrian) +
  #geom_vline(xintercept = c(mondrian$xmin, mondrian$xmax), size = 2, colour = "#262829") +
  #geom_hline(yintercept = c(mondrian$ymin, mondrian$ymax), size = 2, colour = "#262829") +
  geom_linerange(aes(x = xmin, ymin = min(ymin) - 1, ymax = max(ymax) + 1), size = 2, colour = "#262829") +
  geom_linerange(aes(x = xmax, ymin = min(ymin) - 1, ymax = max(ymax) + 1), size = 2, colour = "#262829") +
  geom_linerange(aes(y = ymin, xmin = min(xmin) - 1, xmax = max(xmax) + 1), size = 2, colour = "#262829") +
  geom_linerange(aes(y = ymax, xmin = min(xmin) - 1, xmax = max(xmax) + 1), size = 2, colour = "#262829") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = block_col), colour = "#262829", size = 2) +
  geom_richtext(aes(x = 1947, y = 1965, 
                label = "The 5 artworks in the Tate Collection by Dutch painter Piet Mondrian are shown<br>
                Each artwork is represented by a block of colour<br>
                The position of the top-left corner of the block on the x and y-axis<br>
                indicates the year of the artwork and the year of acquisition respectively<br>
                The dimensions of each block represents the width and height of the artwork"),
                hjust = 0, vjust = 1, family = "Helvetica Neue Thin", size = 3.7,
                fill = NA, label.color = NA) +
  geom_richtext(aes(x = 1947, y = 1977, label = "*Title | Year | Width x Height (mm)*"),
                hjust = 0, vjust = 1, family = "Helvetica Neue Bold Italic", size = 4,
                fill = NA, label.color = NA) +
  geom_richtext(aes(x = 1947, y = 1981, label = glue::glue_collapse(text, sep = "<br><br>")),
                hjust = 0, vjust = 1, family = "Helvetica Neue",
                fill = NA, label.color = NA) +
  geom_richtext(aes(x = 1947, y = 2003, label = "*Graphic: @committedtotape | Data: Tate*"),
                hjust = 0, vjust = 1, family = "Helvetica Neue Bold Italic", size = 4,
                fill = NA, label.color = NA) +
  scale_fill_identity() + 
  scale_x_continuous(breaks = mondrian$xmin, labels = mondrian$xmin, expand = c(0.01,0.01)) +
  scale_y_reverse(breaks = mondrian$ymin, labels = mondrian$ymin, expand = c(0.01,0.01)) +
  labs(title = "Piet Mondrian at The Tate") +
  coord_fixed(clip = "off", xlim = c(1908, 1980)) +
  theme_void(base_family = "Helvetica Neue") +
  theme(axis.text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 0.7),
        plot.title = element_text(size = 28, face = "bold", hjust = 0.5,
                                  margin = margin(10, 0, 20, 0)),
        plot.margin = margin(20,5,20,5))


ggsave(here("2021","week3","piet_mondrian.png"), width = 11, height = 8)






