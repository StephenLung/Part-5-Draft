

# Twitter API
library(rtweet)    

# Interactive Maps
library(tmaptools)
library(leaflet)   

# Shiny
library(shiny)
library(shinythemes)

# Text
library(tidytext)    # Tidy text mining
library(textdata)    # Needed for AFINN

# Visualization
library(plotly)
library(ggwordcloud) # Extension for wordclouds

# Core
library(tidyverse)
library(tidyquant)

source("00_scripts/geocode_for_free.R")



# Twitter API
api_key <- "ryN8dSfophIwOOhTo6hK0P29n"
api_secret_key <- "ellxlgUcw1VtFAAeIWq9KQdBsc5l1z5h5r9GGMIsTEVgRPWKkN"
app_name <- "sentiment_analysis_rtweet"

# 1.0 ACCOUNT SETUP -----

# Twitter Developer Account and App Setup
# Follow these Instructions: https://rtweet.info/articles/auth.html

# ---- API SETUP FOR SHINY ----

# UNCOMMENT THESE LINES AND SAVE YOUR TWITTER APP TOKEN AS AN RDS FILE
# YOU CAN THEN READ THE RDS FILE IF USED ON SHINYAPPS.IO
# KEEP YOUR RDS FILE SAFE. DO !NOT! PUT ON GITHUB. 
# 


token <- create_token(
  
  app             = app_name, 
  consumer_key    = api_key, 
  consumer_secret = api_secret_key
)

token
token %>% write_rds("../my_twitter_token.rds")
token <- read_rds("../my_twitter_token.rds")


# 1.1 MODULARIZING RV_DATA ----
rv_data <- get_stock_list("SP500") %>%  
  slice(2) %>% 
  
  # 1.0 Search based on company name
  # Can be modularized
  str_split(pattern = ",") %>% #splits by the comma
  pluck(1,2) %>% #pick the company name
  str_trim(side = "both")  %>%  #removes whitespace
  
  # 2.0 Search based on ticker symbol 
  # get_symbol_from_user_input() %>% 
  
  search_tweets(
    n = 50,
    lang = "en",
    incude_rts = FALSE
  )

get_stock_list("SP500") %>%  
  slice(2) %>% 
  setup_rv_data(rv_location = rv_location)


setup_rv_data <- function(x, n_tweets = 50){
  x %>% 
    str_split(pattern = ",") %>% #splits by the comma
    pluck(1,2) %>% #pick the company name
    str_trim(side = "both")  %>%  #removes whitespace
    
    # 2.0 Search based on ticker symbol 
    # get_symbol_from_user_input() %>% 
    
    search_tweets(
      n = n_tweets,
      lang = "en",
      incude_rts = FALSE
      #  geocode = rv_location 
    )
}

pull_company <- function(x){
  x %>% 
    str_split(pattern = ",") %>% #splits by the comma
    pluck(1,2) %>% #pick the company name
    str_trim(side = "both")  #removes whitespace
}

# 1.2 MODULARIZING RV_LOCATION ----

rv_location <- "UK, London" %>% 
  geocode_for_free() %>% #extract geo code details
  near_geocode(100) #specify longitude and latitude

"UK, London" %>% 
  setup_rv_location()


setup_rv_location <- function(x, n_miles = 100){
  x %>% 
    geocode_for_free() %>% #extract geo code details
    near_geocode(n_miles) #specify longitude and latitude
}


# 1.3 MODULARIZING RV_SENTIMENT ----

rv_sentiment <- rv_data %>% 
  select(text) %>% 
  rowid_to_column() %>%  
  unnest_tokens(word, text) %>%  #tokenize the text column
  inner_join(get_sentiments("bing"))

get_stock_list("SP500") %>%  
  slice(2) %>% 
  setup_rv_data() %>% 
  setup_rv_sentiment()


setup_rv_sentiment <- function(data){
  data %>% 
    select(text) %>% 
    rowid_to_column() %>%  
    unnest_tokens(word, text) %>%  #tokenize the text column
    inner_join(get_sentiments("bing"))
}

# 1.4 PLOTTING SENTIMENT  ----

plot_sentiment <- function(rv_sentiment_data, plotly = FALSE){
  
  sentiment_by_row_id_tbl <- rv_sentiment_data %>% 
    select(-word) %>%
    count(rowid, sentiment) %>% #counts by rowid the number of appearances of positive, negative
    pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% #fills the blank numbers with 0
    mutate(sentiment = positive - negative) %>%
    left_join(
      rv_data %>% select(screen_name, text) %>% rowid_to_column() #attaches the text data 
    ) 
  
  label_wrap <- label_wrap_gen(width = 60)
  
  data_formatted <- sentiment_by_row_id_tbl %>%
    mutate(text_formatted = str_glue("Row ID: {rowid}
                                     Screen Name: {screen_name}
                                     Text: 
                                     {label_wrap(text)}"))
  
  # Plotting
  g <- data_formatted %>%
    ggplot(aes(rowid, sentiment)) +
    geom_line(color = "#2c3e50", alpha = 0.5) +
    geom_point(aes(text = text_formatted), color = "#2c3e50") +
    geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
    geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
    geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
    geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
    theme_tq() +
    labs(x = "Twitter User", y = "Sentiment",
         title = "Sentiment Analysis of American Airlines stock")
  
  if(plotly)
  g <- ggplotly(g, tooltip = "text") %>%
    layout(
      xaxis = list(
        rangeslider = list(type = "date")
      )
    )
  
  return(g)
  
}

rv_sentiment %>% 
  plot_sentiment(plotly = TRUE)

sentiment_by_row_id_tbl <- rv_sentiment %>% 
  select(-word) %>%
  count(rowid, sentiment) %>% #counts by rowid the number of appearances of positive, negative
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% #fills the blank numbers with 0
  mutate(sentiment = positive - negative) %>%
  left_join(
    rv_data %>% select(screen_name, text) %>% rowid_to_column() #attaches the text data 
  ) 

label_wrap <- label_wrap_gen(width = 60)

data_formatted <- sentiment_by_row_id_tbl %>%
  mutate(text_formatted = str_glue("Row ID: {rowid}
                                     Screen Name: {screen_name}
                                     Text: 
                                     {label_wrap(text)}"))
  
# Plotting
g <- data_formatted %>%
  ggplot(aes(rowid, sentiment)) +
  geom_line(color = "#2c3e50", alpha = 0.5) +
  geom_point(aes(text = text_formatted), color = "#2c3e50") +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
  geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
  geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
  geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
  theme_tq() +
  labs(x = "Twitter User", y = "Sentiment",
       title = "Sentiment Analysis of American Airlines stock")

ggplotly(g, tooltip = "text") %>%
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )


# 1.5 LEAFLET MAP PLOTTING ----

plot_leaflet <- function(rv_location_data){
  # Leaflet Plotting
  tibble(
    location = rv_location
  ) %>%
    separate(location, into = c("lat", "lon", "distance"), sep = ",", remove = FALSE) %>% #separate by ',' and setup a tibble format
    mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>% #removes all non-numeric in the distance column
    mutate_at(.vars = vars(-location), as.numeric) 
  
  data_prepared %>%
    leaflet() %>% #setup on leaflet
    setView(data_prepared$lon, data_prepared$lat, zoom = 3) %>%
    addTiles() %>% #adds a tile
    addMarkers(~lon, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>% #adds a marker
    addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = ~distance/0.000621371)
}

rv_location %>% 
  plot_leaflet()


# Leaflet Plotting
tibble(
  location = rv_location
) %>%
  separate(location, into = c("lat", "lon", "distance"), sep = ",", remove = FALSE) %>% #separate by ',' and setup a tibble format
  mutate(distance = distance %>% str_remove_all("[^0-9.-]")) %>% #removes all non-numeric in the distance column
  mutate_at(.vars = vars(-location), as.numeric) 

data_prepared %>%
  leaflet() %>% #setup on leaflet
  setView(data_prepared$lon, data_prepared$lat, zoom = 3) %>%
  addTiles() %>% #adds a tile
  addMarkers(~lon, ~lat, popup = ~as.character(location), label = ~as.character(location)) %>% #adds a marker
  addCircles(lng = ~lon, lat = ~lat, weight = 1, radius = ~distance/0.000621371) #adds a tile

# Wordcloud 
# tweets_tokenized_tbl <- rv_data %>%
#   select(text) %>%
#   rowid_to_column() %>%
#   unnest_tokens(word, text)
# 
# sentiment_bing_tbl <- tweets_tokenized_tbl %>%
#   inner_join(get_sentiments("bing"))
# 
# sentiment_by_word_tbl <- sentiment_bing_tbl %>%
#   count(word, sentiment, sort = TRUE) 

# 1.6 PLOTTING WORDCLOUD ----

plot_wordcloud <- function(rv_sentiment_data){
  sentiment_by_word_tbl <- rv_sentiment  %>%
    count(word, sentiment, sort = TRUE) 
  
  sentiment_by_word_tbl %>%
    slice(1:100) %>%
    mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
    ggplot(aes(label = word, color = sentiment, size = n)) +
    geom_text_wordcloud_area() + 
    facet_wrap(~ sentiment, ncol = 2) +
    theme_tq(base_size = 30) +
    scale_color_tq() +
    scale_size_area(max_size = 16) 
}

rv_sentiment %>% 
  plot_wordcloud()


sentiment_by_word_tbl <- rv_sentiment  %>%
  count(word, sentiment, sort = TRUE) 

sentiment_by_word_tbl %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() + 
  facet_wrap(~ sentiment, ncol = 2) +
  theme_tq(base_size = 30) +
  scale_color_tq() +
  scale_size_area(max_size = 16) 

# 1.7 DUMPING THE FUNCTIONS ----
dump(c("setup_rv_data", "setup_rv_location", "setup_rv_sentiment", "plot_sentiment", "plot_leaflet", "plot_wordcloud", "pull_company"),
     file = "00_scripts/sentiment_analysis_functions.R")


# ----

# 2.0 SEARCH TWEETS ----
# - Poll tweet history that has happened over n-tweets

# 2.1 search_tweets()
tweets_search <- rtweet::search_tweets(
  q = "#aapl",      # Search query
  n = 100,             # Number of results
  lang = "en",         # Language
  include_rts = FALSE  # Don't include retweets if want unique tweets
)


# 3.0 STREAM TWEETS ----
# - Real-time twitter action 

tweets_rt_search <- stream_tweets(
  q = "apple",      # Search query
  timeout = 10,
  n = 100,             # Number of results
  lang = "en",         # Language
  include_rts = FALSE  # Don't include retweets if want unique tweets
)


# 4.0 GEOCODING FILTERS ----
# - Apply geocoding to search tweets
st <- search_tweets(
  q = "#covid19", 
  n = 300, 
  include_rts = FALSE, 
  lang = "en",
  geocode = geocode_for_free("london, uk") %>% near_geocode(100)
)


# 5.0 TIDY TEXT ----
# Tidy the data
tweets_tokenized_tbl <- tweets_search  %>%
  select(text) %>%
  rowid_to_column() %>%
  unnest_tokens(word, text)

tweets_tokenized_tbl    

tweets_tokenized_tbl %>% count(word, sort = TRUE)    

# 6.0 SENTIMENT ANALYSIS ----

# 6.1 Sentiment Dictionaries 

get_sentiments(lexicon = "bing")  # CategoricalPositive / Negative 

get_sentiments(lexicon = "afinn") # Assigns polarity

# 6.2 Joining Sentiment Dictionaries with Tokenized Text

sentiment_bing_tbl <- tweets_tokenized_tbl %>%
  inner_join(get_sentiments("bing")) #join to only

# 6.3 Measuring Sentiment

# Overall Sentiment
sentiment_bing_tbl %>% count(sentiment)

# Sentiment by user
sentiment_by_row_id_tbl <- sentiment_bing_tbl %>%
  select(-word) %>%
  count(rowid, sentiment) %>% # counts number of negative/positive 
  pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>% #pivot to see by row id
  mutate(sentiment = positive - negative) %>% #complete a calculation of the difference
  left_join(
    tweets_covid %>% select(screen_name, text) %>% rowid_to_column() #join to the twitter text and screen_name
  ) 


# 7.0 POLARITY VISUALIZATION -----

label_wrap <- label_wrap_gen(width = 60) # Wraps the text to show it wrapped instead of one long line

data_formatted <- sentiment_by_row_id_tbl %>% 
  mutate(text_formatted = str_glue("Row ID: {rowid}
                                     Screen Name: {screen_name}
                                     Text: 
                                     {label_wrap(text)}")) #hover text 

g <- data_formatted %>%
  ggplot(aes(rowid, sentiment)) +
  
  geom_line(color = "#2c3e50", alpha = 0.5) +
  geom_point(aes(text = text_formatted), color = "#2c3e50") +
  
  geom_smooth(method = "loess", span = 0.25, se = FALSE, color = "blue") +
  
  geom_hline(aes(yintercept = mean(sentiment)), color = "blue") +
  geom_hline(aes(yintercept = median(sentiment) + 1.96*IQR(sentiment)), color = "red") +
  geom_hline(aes(yintercept = median(sentiment) - 1.96*IQR(sentiment)), color = "red") +
  theme_tq() +
  labs(title = "Sentiment Polarity", x = "Twitter User", y = "Sentiment")

ggplotly(g, tooltip = "text") %>%
  layout(
    xaxis = list(
      rangeslider = list(type = "date")
    )
  )


# 8.0 WORDCLOUD -----

sentiment_by_word_tbl <- sentiment_bing_tbl %>%
  count(word, sentiment, sort = TRUE) #calculate the word frequency by sentiment 


# sentiment_by_word_tbl %>%
#     pivot_wider(names_from = sentiment, values_from = n, values_fill = list(n = 0)) %>%
#     column_to_rownames(var = "word") %>%
#     comparison.cloud(
#         colors = palette_light()
#     )

sentiment_by_word_tbl %>%
  slice(1:100) %>%
  mutate(sentiment = factor(sentiment, levels = c("positive", "negative"))) %>%
  ggplot(aes(label = word, color = sentiment, size = n)) +
  geom_text_wordcloud_area() +  # creates the word cloud
  facet_wrap(~ sentiment, ncol = 2) +
  theme_tq() +
  scale_color_tq() +
  scale_size_area(max_size = 16) +
  labs(title = "Sentiment Word Frequency")






