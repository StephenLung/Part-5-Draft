# INPUT DATA ----
symbols <- c("AAPL",
             "MSFT",
             "NFLX",
             "AMZN")

end     <- today()
start   <- end - years(5) + days(1)

w <- c(0.25, 0.25, 0.25, 0.25)

wts_tbl <- tibble(symbols, w)

# Setup Quandl Authentication
library(pacman)
p_load(tidyquant, 
       alphavantager)

quandl_api_key("https://www.quandl.com/api/v3/datatables/ZACKS/MKTV?api_key=SrqRew7QsQA2qqLjQfKL")



tq_get("ZACKS/MKTV", get = "quandl.datatable")

features_short <- c("Div_Yld", "Eps", "Mkt_Cap_12M_Usd", "Mom_11M_Usd", 
                    "Ocf", "Pb", "Vol1Y_Usd")

tq_get("ZACKS/FC", get = "quandl.datatable")   # Zacks Fundamentals Condensed
tq_get("ZACKS/FR", get = "quandl.datatable")   # Zacks Fundamental Ratios
tq_get("ZACKS/MT", get = "quandl.datatable")   # Zacks Master Table
tq_get("ZACKS/MKTV", get = "quandl.datatable") # Zacks Market Value Supplement
tq_get("ZACKS/SHRS", get = "quandl.datatable") # Zacks Shares Out Supplement


av_api_key("GAYGM4RGVCESAHMN")

#Importing FF factors ----

p_load(tidyverse)

link_5_FF = "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/Developed_5_Factors_CSV.zip"
file_name_5_FF = "Developed_5_Factors.csv"

# Function setup to pull fama french factors
import_5_FF <- function(link, file_name){
  temp <- tempfile()
  
  download.file(link,
                temp,
                quiet = TRUE)
  
  Global_5_Factors <- read_csv(unz(temp, file_name), skip =6) %>% 
    rename(date = X1) %>% 
    mutate(date = ymd(parse_date_time(date, "%Y%m"))) %>% 
    mutate_at(vars(-date), as.numeric) %>% 
    mutate_if(is.numeric, funs(./100)) %>% 
    rename(MKT = `Mkt-RF`) %>% 
    na.omit()
  
  return(Global_5_Factors)
}

Global_5_Factors <- import_5_FF(link_5_FF, file_name_5_FF) 
Global_5_Factors %>% tail()

# Pulling stock data
portfolio_price_data <- multi_asset_price_portfolio(symbols, end, start, wts_tbl) 
portfolio_price_data

masterdata <- inner_join(Global_5_Factors, portfolio_price_data)

masterdata %>% 
  select(-symbol) %>% 
  group_by(date) %>% 
  summarise_all(funs(cor(., adjusted)))
