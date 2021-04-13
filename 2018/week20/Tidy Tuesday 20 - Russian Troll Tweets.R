# Tidy Tuesday Week 20 - Russian Troll Tweets

library(tidyverse)
library(lubridate)
library(tidytext)
library(scales)
library(paletteer)

# character vector of the 9 csv files to be imported
files <- paste0("https://github.com/fivethirtyeight/russian-troll-tweets/raw/master/IRAhandle_tweets_",c(1:9),".csv")
files

# map_df will read in the 9 files and combine into 1 dataframe
df <- files %>% map_df(~read_csv(.))

# confirm frequency is same as data in article
count(df, account_category)

# focus on left and right categories, english language tweets, exclude retweets
# Only look at 2-year period 2015-2017, date period slightly off to allow
# for weekly plotting later
lr_tweets <- df %>% 
  filter(language == "English", account_category %in% c("RightTroll", "LeftTroll"),
         retweet == 0) %>% 
  mutate(publish_date = mdy_hm(publish_date),
         pub_date = as.Date(publish_date)) %>% 
  filter(between(pub_date,as.Date("2015-01-04"), as.Date("2018-01-06"))) %>% 
  select(author, content, region, publish_date, following, followers, account_category,
         account_type, pub_date)

# tidy text analysis
# get data tidy - 1 row per word
tweet_tidy <- lr_tweets %>% 
  unnest_tokens(word, content)

# top 10 negative and positive words for each account category
top_words <- tweet_tidy %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(account_category, word, sentiment) %>% 
  group_by(account_category, sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n))

# plot top 10 words for each sentiment for Right trolls  
ggplot(filter(top_words,account_category == "RightTroll"), aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

# plot top 10 words for each sentiment for left trolls  
ggplot(filter(top_words,account_category == "LeftTroll"), aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip()

# the word 'breaking' is used a lot by LeftTrolls?  
lr_tweets %>% filter(str_detect(content, "breaking")) %>% select(content)

# 'trump' is categorised as positive sentiment, but is most likely referring to Mr Donald
# so let's remove it from below sentiment analysis

# sentiment over time
sentiment_time <- tweet_tidy %>% 
  mutate(date = floor_date(pub_date, unit = "1 week")) %>% 
  group_by(date, account_category) %>% 
  mutate(total_words = n()) %>% 
  ungroup() %>% 
  inner_join(get_sentiments("bing")) %>% 
  anti_join(data_frame(word = "trump"))

# aggregate sentiment by week and category for plotting
sentiment_for_plot <- sentiment_time %>% 
  count(date, account_category, sentiment, total_words) %>% 
  mutate(percent = n / total_words,
         sentiment = case_when(sentiment == "negative" ~ "Negative",
                               TRUE ~ "Positive"))
  

# set date breaks for x-axis in plot
dbs <- seq(as.Date("2015-01-01"), as.Date("2018-01-01"), "6 months")

# create df of important dates for adding to plot
key_dates <- data_frame(account_category = c(rep(c("LeftTroll" ,"RightTroll"), 3), "RightTroll"),
                        date = as.Date(c("2016-07-28", "2016-07-21", "2016-11-08", "2016-11-08",
                                         "2017-01-20", "2017-01-20", "2015-06-16")),
                        percent = rep(0.12, 7),
                        comm = c("Clinton Nominated", "Trump Nominated", "Election Day", "", 
                                 "Inauguration Day", "", "Trump declares\ncandidacy"))

# palette for plot
pal <- paletteer_d(NineteenEightyR, sonny)

# plot sentiment over time
ggplot(sentiment_for_plot, aes(x = date, y = percent)) +
  geom_line(aes(colour = sentiment), size = 1.2) +
  scale_y_continuous(labels = percent) +
  scale_x_date(breaks = dbs, date_labels = "%b '%y", expand = c(0.02, 0.02)) +
  scale_color_manual(values = pal[c(4,1)]) +
  facet_wrap(~ account_category, nrow = 2) +
  labs(x = "Date", y = "% of words attributed to sentiment", colour = "",
       title = "Russian Troll Tweets - Sentiment by Week (Positive/Negative) 2015 - 2017",
       subtitle = "Tweets sent by accounts categorised as 'Left Troll' or 'Right Troll' (Retweets excluded)",
       caption = "SOURCE: Darren Linvill and Patrick Warren") +
  coord_cartesian(xlim = c(as.Date("2015-01-04"), as.Date("2017-12-31"))) +
  geom_vline(data = key_dates, aes(xintercept = date), linetype = 2, size = 0.4) +
  geom_text(data = key_dates, aes(x = date, y = percent, label = comm), 
            hjust = 1, vjust = 1.5, size = 3.5, angle = 90) +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA, colour = "black"),
        strip.background = element_rect(fill = pal[2]),
        strip.text = element_text(colour = "black", face = "bold", size = 12),
        legend.position = c(0.9, 0.9),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "gray97"),
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.background = element_rect(fill = "gray97"),
        panel.grid = element_line(colour = "gray85"))

# save above plot
ggsave("Russian Troll Tweets Weekly sentiment.png", width = 12, height = 7)  
