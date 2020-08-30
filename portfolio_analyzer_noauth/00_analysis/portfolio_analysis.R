
p_load(dplyr,
       rlang)

symbols <- c("VTI", "TLT", "IEF", "GLD", "DBC")
tech_symbols <- c("FB", "AMZN", "AAPL", "NFLX", "GOOG")
end     <- today()
start   <- end - years(2) + days(1)
w       <- c(0.3,
             0.4,
             0.15,
             0.075,
             0.075)
wts_tbl <- tibble(symbols, w)
window <- 24

benchmark_symbols <- "^GSPC"
benchmark_w <- 1
benchmark_tbl <- tibble(benchmark_symbols,
                        benchmark_w)
rfr <- .0003 #risk free rate 0.3% - 10 year treasury rate

# Test sample
"AAPL" %>%
single_asset_price(end = end,
                   start = start) %>% 
  stock_mavg_calculation() %>% 
  plot_stock_mavg()
