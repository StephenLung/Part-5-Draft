plot_portfolio_price <-
function(data, ggplot = TRUE){
  
  p <- data %>% 
    mutate(label_text = str_glue("Date: {date}
                               Price: {scales::dollar(adjusted)}")) %>% 
    mutate(symbol = fct_reorder(symbol, desc(adjusted))) %>% 
    ggplot(aes(date, adjusted, colour = symbol, group = symbol)) + 
    geom_point(aes(text = label_text), size = 0.1)+
    geom_line(lwd = 0.5) +
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Stock prices of the following: PLACEHOLDER"),
         x = "",
         y = "Adjusted Stock Price (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
}
plot_portfolio_index <-
function(data, ggplot = TRUE){
  
  p <- data %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
    
    # pivot_longer(cols = investment.growth:mavg_long, names_to = "legend", values_to = "value",
    #              names_ptypes = list(legend = factor(levels = c("investment.growth", 
    #                                                             "mavg_short",
    #                                                             "mavg_long")))) %>% 
    
    ggplot(aes(date, investment.growth)) + 
    geom_point(aes(text = label_text), size = 0.1)+
    geom_line(lwd = 1.0) +
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() + 
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Portfolio Growth"),
         x = "",
         y = "Adjusted Portfolio Value (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
  
}
mavg_calculation <-
function(data, mavg_short = 20,
                             mavg_long = 50){
  
  data %>% 
    mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
           mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right"))
  
}
plot_portfolio_index_mavg <-
function(data, ggplot = TRUE){
  
  p <- data %>%

    # mutate(mavg_short = rollmean(investment.growth, k = mavg_short, fill = NA, align = "right"),
    # mavg_long = rollmean(investment.growth, k = mavg_long, fill = NA, align = "right")) %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}
                               SMA: {scales::dollar(mavg_short)}
                               LMA: {scales::dollar(mavg_long)}")) %>% 
    pivot_longer(cols = investment.growth:mavg_long, names_to = "legend", values_to = "value",
                 names_ptypes = list(legend = factor(levels = c("investment.growth",
                                                                "mavg_short",
                                                                "mavg_long")))) %>% 
    ggplot(aes(date, value, color = legend, group = legend)) +
    geom_point(aes(text = label_text), size = 0.1) + 
    geom_line(lwd = 1) + 
    theme_tq() + 
    theme(legend.position = "right") +
    scale_color_tq() +
    scale_y_continuous(labels = scales::dollar_format(largest_with_cents = 10),
                       breaks = scales::pretty_breaks()) + 
    labs(title = str_glue("Portfolio Growth"),
         x = "",
         y = "Adjusted Portfolio Value (incl dividends)")
  
  if(ggplot){
    ggplotly(p, tooltip = "text") %>% 
      layout(xaxis = list(
        rangeslider = list(type = "date")
      ))
  }
    
  
}
