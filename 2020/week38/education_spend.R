library(tidyverse)
library(gt)
library(here)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

# focus just on primary and secondary school spending
school_spend <- kids %>% 
  filter(variable == "PK12ed") %>% 
  select(state, year, inf_adj_perchild)

# function for plotting spark lines
plot_group <- function(name, df) {
  plot_object <-
    ggplot(data = df,
           aes(x = year, y = inf_adj_perchild)) +
             geom_line(colour = "#F3FCF0", size = 12) +
             theme_void() +
             # had to lighten backgrund color for export to png as it was darker than table background - weird
             theme(plot.background = element_rect(fill = colorspace::lighten("#47745A", 0.12), colour = colorspace::lighten("#47745A", 0.12)),
                   plot.margin = margin(0,0,0,0))
  return(plot_object)
}

# df of plot for each state
sparklines <-
  school_spend %>%
  dplyr::group_by(state) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    plot = map2(state, data, plot_group)) %>% 
  select(-data)

# prepare data for gt
# join on plotting data from above
# empty variable is needed, see:
# https://github.com/rstudio/gt/issues/152 and
# https://stackoverflow.com/questions/61741440/is-there-a-way-to-embed-a-ggplot-image-dynamically-by-row-like-a-sparkline-usi
table_prepped <- school_spend %>% 
  filter(year %in% c(1997, 2016)) %>% 
  pivot_wider(names_from = year, values_from = inf_adj_perchild) %>% 
  mutate(percent_change = (`2016` - `1997`) / `1997`) %>% 
  inner_join(sparklines, by = "state") %>% 
  mutate(ggplot = NA)

# just keep top 10
top10 <- table_prepped %>% 
  arrange(-`2016`) %>% 
  head(10)

# table! - reference: https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/
gt_table <- top10 %>% 
  gt() %>% 
  fmt_currency(
    columns = vars(`1997`,`2016`),
    decimals = 1,
    pattern = "{x}k"
  ) %>%
  fmt_percent(
    columns = vars(percent_change),
    decimals = 0
  ) %>%
  cols_align(
    align = "right",
    columns = vars(`1997`,`2016`)
  ) %>% 
  cols_label(
    state = "State",
    percent_change = "% Change",
    ggplot = "Trend"
  ) %>%
  tab_source_note(html("<span style='color:#F2AA99;'>TABLE:</span> @committedtotape | <span style='color:#F2AA99;'>DATA:</span> Urban Institute")) %>%
  tab_header(
    title = html("<br><span style='color:#F2AA99;font-size:20pt'>TOP OF THE CLASS</span><br>Public Spending on Elementary & Secondary Education"),
    subtitle = html("Dollars spent per child (inflation adjusted) in the US By State, 1997-2016<br><br><span style='color:#F2AA99;'>Top 10 Spenders in 2016</span>")
  ) %>% 
  # Adjust title font
  tab_style(
    style = 
      cell_text(
        font = "Chalkduster",
        color = "#8DDBE0",
        weight = "bold",
        align = "center"),
    locations = cells_title(groups = "title")
  ) %>%
  # Adjust sub-title font
  tab_style(
    style = cell_text(
        font = "Chalkduster",
        align = "center"),
    locations = cells_title(groups = "subtitle")
  ) %>% 
  # Style header font
  gt::tab_style(
    style = cell_text(font = "Chalkduster", 
                      weight = "bold", 
                      color = "#FFD23F"),
    locations = cells_column_labels(gt::everything())
  ) %>%
  # borders
  tab_style(
    style = cell_borders(
        sides = c("bottom", "top"),
        color = "white",
        weight = px(5)
    ),
    locations = cells_column_labels(columns = gt::everything())
  ) %>% 
  tab_style(
    style = cell_borders(
        sides = "bottom",
        color = "white",
        weight = px(3)),
    locations = cells_body()
  ) %>% 
  # font
  opt_table_font(font = list(c("Chalkduster"))) %>% 
  # table options
  tab_options(table.background.color = "#47745A",
              table.font.color = "#F3FCF0",
              data_row.padding = px(10)
              ) %>% 
  # rendering the spark lines - from gh reference above
  text_transform(
    locations = cells_body(columns = vars(ggplot)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(top10$plot, ggplot_image, height = px(30), aspect_ratio = 4)
    }
  ) %>%
cols_hide(vars(plot))

gt_table

gtsave(gt_table, here("2020", "week38", "education_spend.png"))
#gtsave(gt_table, "education_spend.html")
