library(tidyverse)
library(gt)
library(extrafont)

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

# focus just on primary and secondary school spending
school_spend <- kids %>% 
  filter(variable == "PK12ed") %>% 
  select(state, year, inf_adj_perchild)

# function for plotting spark lines
plot_spark <- function(name, df) {
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

# function for sketchy barplot
plot_bar <- function(name, df, yr) {
  
  barheight <- df %>% 
    filter(year == yr) %>% 
    pull(inf_adj_perchild)
  
  plot_object <-
    tibble( x = runif(8*barheight, -0.01, 0.01), xend = runif(8*barheight, 0.99, 1.01),
            y = runif(8*barheight, 0.1, barheight), yend = y + jitter(0.05),
            y1 = yend, yend1 = y - jitter(1), x1 = xend, xend1 = x) %>% 
    ggplot() +
    geom_curve(aes(x = x, xend = xend, y = y, yend = yend), 
               colour = "white", size = 2, curvature = 0.01, alpha = 0.7) +
    geom_curve(aes(x = x1, xend = xend1, y = y1, yend = yend1), 
               colour = "white", size = 2, curvature = -0.01, alpha = 0.7) +
    scale_y_continuous(limits = c(-0.2,20)) +
    scale_x_continuous(limits = c(-0.2,1.2)) +
    theme_void() +
    #had to lighten backgrund color for export to png as it was darker than table background - weird
    theme(plot.background = element_rect(fill = colorspace::lighten("#47745A", 0.12), colour = colorspace::lighten("#47745A", 0.12)),
          plot.margin = margin(0,0,0,0))
  return(plot_object)
}

# df of plots for each state
plots <-
  school_spend %>%
  dplyr::group_by(state) %>%
  tidyr::nest() %>%
  dplyr::mutate(
    spark = map2(state, data, plot_spark),
    bar97 = map2(state, data, plot_bar, yr = 1997),
    bar16 = map2(state, data, plot_bar, yr = 2016)) %>% 
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
  inner_join(plots, by = "state") %>% 
  mutate(ggplot1 = NA,
         ggplot2 = NA,
         ggplot3 = NA) %>% 
  select(state, `1997`, ggplot1, `2016`, ggplot2, percent_change, ggplot3, bar97, bar16, spark)

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
    ggplot1 = " ",
    ggplot2 = " ",
    ggplot3 = "Trend"
  ) %>%
  tab_source_note(html("<span style='color:#4E3423;font-size:16pt'><b>TABLE: committedtotape | DATA: Urban Institute</b></span>")) %>%
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
  tab_style(
    style = cell_text(font = "Chalkduster"),
    locations = cells_body(columns = gt::everything())
  ) %>%
  #borders
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
  # left hand board frame
  tab_style(
    style = list(
      cell_borders(
        sides = "left",
        color = "#A36734",
        weight = px(20)
      )
    ),
    locations = list(cells_body(columns = vars(state)), cells_column_labels(columns = vars(state)),
                     cells_title("title"), cells_title("subtitle"))
  ) %>%
  # right hand board frame
  tab_style(
    style = list(
      cell_borders(
        sides = "right",
        color = "#A36734",
        weight = px(20)
      )
    ),
    locations = list(cells_body(columns = vars(ggplot3)), cells_column_labels(columns = vars(ggplot3)),
                     cells_title("title"), cells_title("subtitle"))
  ) %>%
  tab_style(
    style = cell_fill(color = "#47745A"),
    locations = cells_body(
      columns = gt::everything())
  ) %>%
  # font
  opt_table_font(font = list(c("Frenchy"))) %>% 
  # table options
  tab_options(table.background.color = "#A36734",
              heading.background.color = "#47745A",
              column_labels.background.color = "#47745A",
              table.font.color = "#F3FCF0",
              data_row.padding = px(10),
              table.border.top.width = px(20),
              table.border.top.color = "#A36734",
              table.border.bottom.color = "#A36734",
              source_notes.background.color = "#A36734"
              #           ,
              #           table.additional_css = ".gt_table {
              # border: #A36A35
              # }"
  ) %>% 
  # rendering bar 1997
  text_transform(
    locations = cells_body(columns = vars(ggplot1)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(top10$bar97, ggplot_image, height = px(50), aspect_ratio = 1.5)
    }
  ) %>%
  # rendering bar 2016
  text_transform(
    locations = cells_body(columns = vars(ggplot2)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(top10$bar16, ggplot_image, height = px(50), aspect_ratio = 1.5)
    }
  ) %>%
  # rendering the spark lines - from gh reference above
  text_transform(
    locations = cells_body(columns = vars(ggplot3)), # use empty cell as location
    fn = function(x) {
      # Insert each image into each empty cell in `ggplot`
      map(top10$spark, ggplot_image, height = px(30), aspect_ratio = 4)
    }
  ) %>%
  cols_hide(vars(bar97, bar16, spark))

gt_table

gtsave(gt_table, "education_spend_1.png")
#gtsave(gt_table, "education_spend.html")
