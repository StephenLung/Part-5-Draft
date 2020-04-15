# INPUT DATA ----
symbols <- c("AAPL",
             "MSFT",
             "NFLX",
             "AMZN")

end     <- today()
start   <- end - years(5) + days(1)

w <- c(0.25, 0.25, 0.25, 0.25)

wts_tbl <- tibble(symbols, w)

source("00_prior_script/wealth_index.R")

# 1.0 GET STOCK DATA ----
portfolio_price_data <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) 

# 1.1 PLOT DATA ----

q <- stock_price_data %>% 
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

ggplotly(q, tooltip = "text")

plot_portfolio_price <- function(data, ggplot = TRUE){
  
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

portfolio_price_data %>% 
  plot_portfolio_price()

# 2.0 PORTFOLIO DATA ----

portfolio_data_tbl <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) %>% 
  multi_asset_return_portfolio(period = "monthly") %>% 
  wealth_index(wts_tbl = wts_tbl, name_portfolio = "test portfolio")

p <- portfolio_data_tbl %>% 
  mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
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

ggplotly(p, tooltip = "text")


plot_portfolio_index <- function(data, ggplot = TRUE){
  
  p <- data %>% 
    mutate(label_text = str_glue("Date: {date}
                               Investment: {scales::dollar(investment.growth)}
                               Growth %: {scales::percent(portfolio.wealthindex-1, accuracy = 0.01)}")) %>% 
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

portfolio_data_tbl %>% 
  plot_portfolio_index()

# 7.0 SAVE SCRIPTS ----

dump(list = c("plot_portfolio_price",
              "plot_portfolio_index"),
     file = "00_scripts/portfolio_analysis_functions.R")

