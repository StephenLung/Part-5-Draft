setup_rv_data <-
function(x, n_tweets = 50){
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
setup_rv_location <-
function(x, n_miles = 100){
  x %>% 
    geocode_for_free() %>% #extract geo code details
    near_geocode(n_miles) #specify longitude and latitude
}
setup_rv_sentiment <-
function(data){
  data %>% 
    select(text) %>% 
    rowid_to_column() %>%  
    unnest_tokens(word, text) %>%  #tokenize the text column
    inner_join(get_sentiments("bing"))
}
plot_sentiment <-
function(rv_sentiment_data, plotly = FALSE){
  
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
plot_leaflet <-
function(rv_location_data){
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
plot_wordcloud <-
function(rv_sentiment_data){
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
pull_company <-
function(x){
  x %>% 
    str_split(pattern = ",") %>% #splits by the comma
    pluck(1,2) %>% #pick the company name
    str_trim(side = "both")  #removes whitespace
}
