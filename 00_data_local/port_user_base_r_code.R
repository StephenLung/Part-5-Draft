
library(lubridate)


user_base_tbl <- tibble(
    user = c("user1", "user2"),
    password = c("pass1", "pass2"),
    permissions = c("admin", "standard"),
    name = c("User One", "User Two"),
    favourites = list(c("AAL", "DAL", "UAL"), c("MA", "V", "FB")),
    # last_symbol = c("GOOG", "NFLX"),
    user_settings = list(tibble(mavg_short = 20, mavg_long = 50, start_date = "2018-01-01", end_date = "2020-01-01"),
                         tibble(mavg_short = 30, mavg_long = 90, start_date = "2015-01-01", end_date = today())),
  account_created = c(as_datetime("2019-05-12 12:31:09"), as_datetime("2019-06-04 06:18:02"))
)

saveRDS(user_base_tbl, "00_data_local/port_user_base_tbl.rds")
